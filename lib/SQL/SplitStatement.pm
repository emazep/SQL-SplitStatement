## no critic
package SQL::SplitStatement;
## use critic

use strict;
use warnings;

use base 'Class::Accessor::Fast';

use Carp qw(croak);
use SQL::Tokenizer 0.20 qw(tokenize_sql);
use List::MoreUtils qw(firstval each_array);
use Regexp::Common qw(delimited);

use constant {
    SEMICOLON     => ';',
    FORWARD_SLASH => '/',
    PLACEHOLDER   => '?',
    DOLLAR_QUOTE  => '$$',
    
    SEMICOLON_TERMINATOR => 1,
    SLASH_TERMINATOR     => 2,
    CUSTOM_DELIMITER     => 3
};

my $transaction_re = qr[^(?:
    ;
    |/
    |WORK
    |TRAN
    |TRANSACTION
    |ISOLATION
    |READ
)$]xi;
my $procedural_END_re     = qr/^(?:IF|LOOP)$/i;
my $terminator_re         = qr[;|/|;\s+/];
my $begin_comment_re      = qr/^(?:--|\/\*)/;
my $quoted_re             = $RE{delimited}{-delim=>q{'"}};
my $DELIMITER_re          = qr/^DELIMITER$/i;
my $DECLARE_re            = qr/^DECLARE$/i;
my $PROCEDURE_FUNCTION_re = qr/^(?:PROCEDURE|FUNCTION)$/i;
my $PACKAGE_re            = qr/^PACKAGE$/i;
my $BEGIN_re              = qr/^BEGIN$/i;
my $END_re                = qr/^END$/i;
my $AS_re                 = qr/^AS$/i;
my $DROP_re               = qr/^DROP$/i;

my $GRANT_REVOKE_re            = qr/^(?:GRANT|REVOKE)$/i;;
my $CREATE_ALTER_re            = qr/^(?:CREATE|ALTER)$/i;
my $OR_REPLACE_re              = qr/^(?:OR|REPLACE)$/i;
my $OR_REPLACE_PACKAGE_BODY_re = qr/^(?:OR|REPLACE|PACKAGE|BODY)$/i;

my $pre_identifier_re = qr/TABLE|[.,(]/i;

SQL::SplitStatement->mk_accessors( qw/
    keep_terminators
    keep_extra_spaces
    keep_empty_statements
    keep_comments
    _tokens
    _current_statement
    _custom_delimiter
    _terminators
    _tokens_in_custom_delimiter
/);

# keep_terminators alias
sub keep_terminator { shift->keep_terminators(@_) }

sub new {
    my $class = shift;
    my $parameters = @_ > 1 ? { @_ } : $_[0] || {};
    if ( exists $parameters->{keep_terminators} ) {
        croak( q[keep_terminator and keep_terminators can't be both assigned'] )
            if exists $parameters->{keep_terminator}
    }
    elsif ( exists $parameters->{keep_terminator} ) {
        $parameters->{keep_terminators} = delete $parameters->{keep_terminator}
    }
    $class->SUPER::new( $parameters )
}

sub split {
    my ($self, $code) = @_;
    my ($statements, undef) = $self->split_with_placeholders($code);
    return @{ $statements }
}

sub split_with_placeholders {
    my ($self, $code) = @_;
    
    my @placeholders = ();
    my @statements   = ();
    my $statement_placeholders = 0;
    my $package_name = '';
    my $prev_token   = '';
    
    my $inside_block        = 0;
    my $inside_declare      = 0;
    my $inside_package      = 0;
    my $inside_dollar       = 0;
    my $inside_grant_revoke = 0;
    
    my $custom_delimiter_def_found = 0;
    
    $code = "\n" if ! defined $code;
    $self->_tokens( [ tokenize_sql($code) ] );
    $self->_terminators( [] ); # Needed (only) to remove them afterwards
                               # when keep_terminators is false.
    
    $self->_current_statement('');
    
    while ( defined( my $token = shift @{ $self->_tokens } ) ) {
        my $terminator_found = 0;
        
        # Skip this token if it's a comment and we don't want to keep it.
        next if $self->_is_comment($token) && ! $self->keep_comments;
        
        # Append the token to the current statement;
        $self->_current_statement( $self->_current_statement . $token );
        
        # The token is gathered even if it was a space-only token,
        # but in this case we can skip any further analysis.
        next if $token =~ /^\s+$/;
        
        if ( $token =~ $DELIMITER_re && ! $prev_token ) {
            my $tokens_to_shift = $self->_shift_and_set_custom_delimiter;
            $self->_current_statement(
                $self->_current_statement
                . join '', splice @{ $self->_tokens }, 0, $tokens_to_shift
            );
            $custom_delimiter_def_found = 1;
            $self->_custom_delimiter(undef)
                if $self->_custom_delimiter eq SEMICOLON
        }
        elsif ( $self->_is_BEGIN_of_block($token, $prev_token) ) {
            $inside_block++;
            $inside_declare = 0
        }
        elsif ( $token eq DOLLAR_QUOTE ) {
            $inside_dollar = $prev_token =~ $AS_re ? 1 : 0
        }
        elsif ( $token =~ $CREATE_ALTER_re ) {
            my $next_token = $self->_peek_at_next_significant_token(
                $OR_REPLACE_re
            );
            if ( $next_token =~ $PACKAGE_re ) {
                $inside_package = 1;
                $package_name = $self->_peek_at_package_name
            }
        }
        elsif ( $token =~ /$DECLARE_re|$PROCEDURE_FUNCTION_re/ ) {
            # In MySQL a declare can only appear inside a BEGIN ... END block.
            $inside_declare = 1
                if !$inside_block
                && !$inside_package
                && $prev_token !~ $DROP_re
                && $prev_token !~ $pre_identifier_re
        }
        elsif ( $token =~ /$GRANT_REVOKE_re/ ) {
            $inside_grant_revoke = 1 unless $prev_token
        }
        elsif (
            defined ( my $name = $self->_is_END_of_block($token) )
        ) {
            # The END of a PACKAGE block may or may not be followed
            # by the package name: we have to detect both these cases.
            if (
                $inside_package && ( $name eq $package_name || ! $inside_block )
            ) {
                $inside_package = 0;
                $package_name = ''
            }
            # This goes *AFTER* the above _is_END_of_block check!
            $inside_block-- if $inside_block
        }
        elsif (
            $token eq PLACEHOLDER
            && ( ! $self->_custom_delimiter
                || $self->_custom_delimiter ne PLACEHOLDER
            )
        ) {
            $statement_placeholders++
        }
        else {
            $terminator_found = $self->_is_terminator($token, $prev_token)
        }
        
        if (
            ! $terminator_found && ! $custom_delimiter_def_found
                ||
            $terminator_found
            && $terminator_found == SEMICOLON_TERMINATOR
            && (
                $inside_block || $inside_declare
                || $inside_package || $inside_dollar
            ) && ! $inside_grant_revoke
        ) {
            $prev_token = $token
                if $token =~ /\S/ && ! $self->_is_comment($token);
            next
        }
        
        # Whenever we get this far, we have a new statement.
        
        push @statements, $self->_current_statement;
        push @placeholders, $statement_placeholders;
        
        # If $terminator_found == CUSTOM_DELIMITER
        # @{ $self->_terminators } element has already been pushed,
        # so we have to set it only in the case tested below.
        push @{ $self->_terminators }, [$terminator_found, undef]
            if (
                $terminator_found == SEMICOLON_TERMINATOR
                || $terminator_found == SLASH_TERMINATOR
            );
        
        $self->_current_statement('');
        $statement_placeholders = 0;
        
        $prev_token          = '';
        $inside_block        = 0;
        $inside_declare      = 0;
        $inside_package      = 0;
        $inside_dollar       = 0;
        $inside_grant_revoke = 0;
        
        $custom_delimiter_def_found = 0
    }
    
    # Last statement.
    push @statements, $self->_current_statement;
    push @{ $self->_terminators }, [undef, undef];
    push @placeholders, $statement_placeholders;
    
    my @filtered_statements;
    my @filtered_terminators;
    my @filtered_placeholders;
    
    if ( $self->keep_empty_statements ) {
        @filtered_statements   = @statements;
        @filtered_terminators  = @{ $self->_terminators };
        @filtered_placeholders = @placeholders
    } else {
        my $sp = each_array(
            @statements, @{ $self->_terminators }, @placeholders
        );
        while ( my ($statement, $terminator, $placeholder_num) = $sp->() ) {
            my $only_terminator_re
                = $terminator->[0] && $terminator->[0] == CUSTOM_DELIMITER
                ? qr/^\s*$terminator->[1]?\s*$/
                : qr/^\s*$terminator_re?\s*$/;
            unless ( $statement =~ $only_terminator_re ) {
                push @filtered_statements, $statement;
                push @filtered_terminators, $terminator;
                push @filtered_placeholders, $placeholder_num
            }
        }
    }
    
    unless ( $self->keep_terminators ) {
        for ( my $i = 0; $i < @filtered_statements; $i++ ) {
            my $terminator = $filtered_terminators[$i];
            if ( $terminator->[0] && $terminator->[0] == CUSTOM_DELIMITER ) {
                $filtered_statements[$i] =~ s/$terminator->[1]$//
            } elsif ( $terminator->[0] ) {
                $filtered_statements[$i] =~ s/$terminator_re$//
            }
        }
    }
    
    unless ( $self->keep_extra_spaces ) {
        s/^\s+|\s+$//g foreach @filtered_statements
    }
    
    return ( \@filtered_statements, \@filtered_placeholders )
}

sub _is_comment {
    my ($self, $token) = @_;
    return $token =~ $begin_comment_re
}

sub _is_BEGIN_of_block {
    my ($self, $token, $prev_token) = @_;
    return 
        $token =~ $BEGIN_re
        && $prev_token !~ $pre_identifier_re
        && $self->_peek_at_next_significant_token !~ $transaction_re
}

sub _is_END_of_block {
    my ($self, $token) = @_;
    my $next_token = $self->_peek_at_next_significant_token;
    
    # Return possible package name.
    if (
        $token =~ $END_re && (
            ! defined($next_token)
            || $next_token !~ $procedural_END_re
        )
    ) { return defined $next_token ? $next_token : '' }
    
    return
}

sub _peek_at_package_name {
    shift->_peek_at_next_significant_token($OR_REPLACE_PACKAGE_BODY_re)
}

sub _shift_and_set_custom_delimiter {
    my $self = shift;
    
    my $tokens = $self->_tokens;
    
    my $base_index = 0;
    $base_index++ while $tokens->[$base_index] =~ /^\s$/;
    
    my $first_token_in_delimiter = $tokens->[$base_index];
    my $delimiter = '';
    my $tokens_in_delimiter;
    my $tokens_to_shift;
    
    if ( $first_token_in_delimiter =~ $quoted_re ) {
        # Quoted custom delimiter: it's just a single token (to shift)...
        $tokens_to_shift = $base_index + 1;
        # ... However it can be composed by several tokens
        # (according to SQL::Tokenizer), once removed the quotes.
        $delimiter = substr $first_token_in_delimiter, 1, -1;
        $tokens_in_delimiter =()= tokenize_sql($delimiter)
    } else {
        # Gather an unquoted custom delimiter, which could be composed
        # by several tokens (that's the SQL::Tokenizer behaviour).
        foreach ( @{$tokens}[ $base_index .. $#{ $tokens } ] ) {
            last if /^\s$/;
            $delimiter .= $_;
            $tokens_in_delimiter++
        }
        $tokens_to_shift = $base_index + $tokens_in_delimiter
    }
    
    $self->_custom_delimiter($delimiter);
    
    # We've just found a custom delimiter definition,
    # which means that this statement has no (additional) terminator,
    # therefore we won't have to delete anything.
    push @{ $self->_terminators }, [undef, undef];
    
    $self->_tokens_in_custom_delimiter($tokens_in_delimiter);
    
    return $tokens_to_shift
}

sub _is_custom_delimiter {
    my ($self, $token) = @_;
    
    my $tokens = $self->_tokens;
    my @delimiter_tokens
        = splice @{$tokens}, 0, $self->_tokens_in_custom_delimiter() - 1;
    my $lookahead_delimiter = join '', @delimiter_tokens;
    if ( $self->_custom_delimiter eq $token . $lookahead_delimiter ) {
        $self->_current_statement(
            $self->_current_statement . $lookahead_delimiter
        );
        push @{ $self->_terminators },
            [ CUSTOM_DELIMITER, $self->_custom_delimiter ];
        return 1
    } else {
        unshift @{$tokens}, @delimiter_tokens;
        return
    }
}

sub _is_terminator {
    my ($self, $token, $prev_token) = @_;
    
    # This is the first test to perform!
    if ( $self->_custom_delimiter ) {
        # If a custom delimiter is currently defined,
        # no other token can terminate a statement.
        return CUSTOM_DELIMITER if $self->_is_custom_delimiter($token);
        return
    }
    
    return if $token ne FORWARD_SLASH && $token ne SEMICOLON;
    
    if ( $token eq FORWARD_SLASH ) {
        return SLASH_TERMINATOR if $prev_token eq SEMICOLON;
        return SEMICOLON_TERMINATOR
    }
    
    # $token eq SEMICOLON.
    my $next_token = $self->_peek_at_next_significant_token;
    return SEMICOLON_TERMINATOR
        if ! defined($next_token) || $next_token ne FORWARD_SLASH;
    
    # $next_token eq FORWARD_SLASH: let's wait for it to terminate.
    return
}

sub _peek_at_next_significant_token {
    my ($self, $skiptoken_re) = @_;
    
    my $tokens = $self->_tokens;
    return $skiptoken_re
        ? firstval {
            /\S/ && ! $self->_is_comment($_) && ! /$skiptoken_re/
        } @{ $tokens }
        : firstval {
            /\S/ && ! $self->_is_comment($_)
        } @{ $tokens }
}

1;

__END__

=head1 NAME

SQL::SplitStatement - Split any SQL code into atomic statements

=head1 SYNOPSIS

    # Multiple SQL statements in a single string
my $sql_code = <<'SQL';
    CREATE TABLE parent(a, b, c   , d    );
    CREATE TABLE child (x, y, "w;", "z;z");
    /* C-style comment; */
    CREATE TRIGGER "check;delete;parent;" BEFORE DELETE ON parent WHEN
        EXISTS (SELECT 1 FROM child WHERE old.a = x AND old.b = y)
    BEGIN
        SELECT RAISE(ABORT, 'constraint failed;'); -- Inlined SQL comment
    END;
    -- Standalone SQL; comment; with semicolons;
    INSERT INTO parent (a, b, c, d) VALUES ('pippo;', 'pluto;', NULL, NULL);
SQL
    
    use SQL::SplitStatement;
    
    my $sql_splitter = SQL::SplitStatement->new;
    my @statements = $sql_splitter->split($sql_code);
    
    # @statements now is:
    #
    # (
    #     'CREATE TABLE parent(a, b, c   , d    )',
    #     'CREATE TABLE child (x, y, "w;", "z;z")',
    #     'CREATE TRIGGER "check;delete;parent;" BEFORE DELETE ON parent WHEN
    #     EXISTS (SELECT 1 FROM child WHERE old.a = x AND old.b = y)
    # BEGIN
    #     SELECT RAISE(ABORT, \'constraint failed;\');
    # END',
    #     'INSERT INTO parent (a, b, c, d) VALUES (\'pippo;\', \'pluto;\', NULL, NULL)'
    # )

=head1 DESCRIPTION

This is a simple module which tries to split any SQL code, even including
non-standard extensions (for the details see the L</SUPPORTED DBMSs> section
below), into the atomic statements it is composed of.

The logic used to split the SQL code is more sophisticated than a raw C<split>
on the C<;> (semicolon) character: first, various different statement terminator
I<tokens> are recognized (see below for the list), then this module is able to
correctly handle the presence of said tokens inside identifiers, values,
comments, C<BEGIN ... END> blocks (even nested), I<dollar-quoted> strings, MySQL
custom C<DELIMITER>s, procedural code etc., as (partially) exemplified in the
L</SYNOPSIS> above.

Consider however that this is by no means a validating parser (technically
speaking, it's just a I<context-sensitive tokenizer>). It should rather be seen
as an in-progress I<heuristic> approach, which will gradually improve as bugs
will be reported. This also means that, with the exception of the
L</LIMITATIONS> detailed below, there are no known (to the author) SQL
constructs the most current release of this module can't handle.

If your atomic statements are to be fed to a DBMS, you are encouraged to use
L<DBIx::MultiStatementDo> instead, which uses this module and also (optionally)
offers automatic transactions support, so that you'll have the I<all-or-nothing>
behavior you would probably want.

=head1 METHODS

=head2 C<new>

=over 4

=item * C<< SQL::SplitStatement->new( %options ) >>

=item * C<< SQL::SplitStatement->new( \%options ) >>

=back

It creates and returns a new SQL::SplitStatement object. It accepts its options
either as a hash or a hashref.

C<new> takes the following Boolean options, which all default to false.

=over 4

=item * C<keep_terminators>

A Boolean option which causes, when set to a false value (which is the default),
the trailing terminator tokens to be discarded in the returned atomic
statements.
When set to a true value, the terminators are kept instead.

If your statements are to be fed to a DBMS, you are advised to keep this option
to its default (false) value, since some drivers/DBMSs don't want the terminator
to be present at the end of the (single) statement.

The strings currently recognized as terminators (depending on the I<context>)
are:

=over 4

=item * C<;> (the I<semicolon> character).

=item * C</> (the I<forward-slash> character).

=item * A semicolon followed by a forward-slash on its own line. This latter
string is treated as a single token (it is used to terminate PL/SQL procedures).

=item * Any string defined by the MySQL C<DELIMITER> command.

=back

(Note that the last, possibly empty, statement of a given SQL text, never has a
trailing terminator. See below for an example.)

=item * C<keep_terminator>

An alias for the the C<keep_terminators> option explained above.
Note that if C<keep_terminators> and C<keep_terminator> are both set at object
construction time, C<new> throws an exception.

=item * C<keep_extra_spaces>

A Boolean option which causes, when set to a false value (which is the default),
the spaces (C<\s>) around the statements to be trimmed.
When set to a true value, these spaces are kept instead.

When C<keep_terminators> is set to false as well, the terminator is discarded
first (regardless of the spaces around it) and the trailing spaces are trimmed
then. This ensures that if C<keep_extra_spaces> is set to false, the returned
statements will never have trailing (nor leading) spaces, regardless of the
C<keep_terminators> value.

=item * C<keep_comments>

A Boolean option which causes, when set to a false value (which is the default),
the comments to be discarded in the returned statements. When set to a true
value, they are kept with the statements instead.

Both SQL and multi-line C-style comments are recognized.

When kept, each comment is returned in the same string with the atomic statement
it belongs to. A comment belongs to a statement if it appears, in the original
SQL code, before the end of that statement and after the terminator of the
previous statement (if it exists), as shown in this pseudo-SQL snippet:

    /* This comment
    will be returned
    together with statement1 */
    
    <statement1>; -- This will go with statement2
                  -- (note the semicolon which closes statement1)
    
    <statement2>
    -- This with statement2 as well

=item * C<keep_empty_statements>

A Boolean option which causes, when set to a false value (which is the default),
the empty statements to be discarded. When set to a true value, the empty
statements are returned instead.

A statement is considered empty when it contains no characters other than the
terminator and space characters (C<\s>).

A statement composed solely of comments is not recognized as empty and may
therefore be returned even when C<keep_empty_statements> is false. To avoid
this, it is sufficient to leave C<keep_comments> to false as well.

Note instead that an empty statement is recognized as such regardless of the
value of the options C<keep_terminators> and C<keep_extra_spaces>.

=back

These options are basically to be kept to their default (false) values,
especially if the atomic statements are to be given to a DBMS.

They are intended mainly for I<cosmetic> reasons, or if you want to count by how
many atomic statements, including the empty ones, your original SQL code was
composed of.

Another situation where they are useful (in the general case necessary, really),
is when you want to retain the ability to verbatim rebuild the original SQL
string from the returned statements:

    my $verbatim_splitter = SQL::SplitStatement->new(
        keep_terminators      => 1,
        keep_extra_spaces     => 1,
        keep_comments         => 1,
        keep_empty_statements => 1
    );
    
    my @verbatim_statements = $verbatim_splitter->split($sql_string);
    
    $sql_string eq join '', @verbatim_statements; # Always true, given the constructor above.

Other than this, again, you are highly recommended to stick with the defaults.

=head2 C<split>

=over 4

=item * C<< $sql_splitter->split( $sql_string ) >>

=back

This is the method which actually splits the SQL code into its atomic
components.

It returns a list containing the atomic statements, in the same order they
appear in the original SQL code. The atomic statements are returned according to
the options explained above.

Note that, as mentioned above, an SQL string which terminates with a terminator
token (for example a semicolon), contains a trailing empty statement: this is
correct and it is treated accordingly (if C<keep_empty_statements> is set to a
true value):

    my $sql_splitter = SQL::SplitStatement->new(
        keep_empty_statements => 1
    );
    
    my @statements = $sql_splitter->split( 'SELECT 1;' );
    
    print 'The SQL code contains ' . scalar(@statements) . ' statements.';
    # The SQL code contains 2 statements.

=head2 C<split_with_placeholders>

=over 4

=item * C<< $sql_splitter->split_with_placeholders( $sql_string ) >>

=back

It works exactly as the C<split> method explained above, except that it returns
also a list of integers, each of which is the number of the
I<unnamed placeholders> contained in the corresponding atomic statement.

More precisely, its return value is a list of two elements, the first of which
is a reference to the list of the atomic statements exactly as returned by the
C<split> method, while the second is a reference to the list of the number of
placeholders as explained above.

Currently the only recognized placeholders are the C<?> (question mark)
characters.

Here is an example:

    # 4 statements (valid SQLite SQL)
my $sql_code = <<'SQL';
    CREATE TABLE state (id, name);
    INSERT INTO  state (id, name) VALUES (?, ?);
    CREATE TABLE city  (id, name, state_id);
    INSERT INTO  city  (id, name, state_id) VALUES (?, ?, ?)
SQL
    
    my $splitter = SQL::SplitStatement->new;
    
    my ( $statements, $placeholders )
        = $splitter->split_with_placeholders( $sql_code );
    
    # $placeholders now is: [0, 2, 0, 3]

where the returned C<$placeholders> list(ref) is to be read as follows: the
first statement contains 0 placeholders, the second 2, the third 0 and the
fourth 3.

=head2 C<keep_terminators>

=over 4

=item * C<< $sql_splitter->keep_terminators >>

=item * C<< $sql_splitter->keep_terminators( $boolean ) >>

Getter/setter method for the C<keep_terminators> option explained above.

=back

=head2 C<keep_terminator>

An alias for the C<keep_terminators> method explained above.

=head2 C<keep_extra_spaces>

=over 4

=item * C<< $sql_splitter->keep_extra_spaces >>

=item * C<< $sql_splitter->keep_extra_spaces( $boolean ) >>

Getter/setter method for the C<keep_extra_spaces> option explained above.

=back

=head2 C<keep_comments>

=over 4

=item * C<< $sql_splitter->keep_comments >>

=item * C<< $sql_splitter->keep_comments( $boolean ) >>

Getter/setter method for the C<keep_comments> option explained above.

=back

=head2 C<keep_empty_statements>

=over 4

=item * C<< $sql_splitter->keep_empty_statements >>

=item * C<< $sql_splitter->keep_empty_statements( $boolean ) >>

Getter/setter method for the C<keep_empty_statements> option explained above.

=back

=head1 SUPPORTED DBMSs

SQL::SplitStatement aims to cover the widest possible range of DBMSs, SQL
dialects and extensions (even proprietary), in a fully transparent way for the
user.

Currently it has been tested mainly on SQLite, PostgreSQL, MySQL and Oracle.

=head1 LIMITATIONS

To be split correctly, the given SQL code is subject to the following
limitations, mainly concerning procedural code (the limitation about the use
of some keywords as unquoted identifiers affecting the previous releases, has
now been eliminated).

=over 4

=item * Procedural extensions

Currently any block of code which start with C<DECLARE>, C<CREATE> or C<CALL>
is correctly recognized, as well as I<bare> C<BEGIN ... END> blocks and
I<dollar quoted> blocks, therefore a wide range of procedural extensions should
be handled correctly. However, only PL/SQL, PL/PgSQL and MySQL code has been
tested so far.

If you need also other procedural languages to be recognized, please let me know
(possibly with some test cases).

=item * PL/SQL

If a I<package> contains also an I<initialization block>, then it must terminate
with a semicolon and a slash, or it must have the package name after the C<END>
of package (which is the recommended practice anyway).

For example, these two package (pseudo-)definitions will be correctly split:

    -- OK since it has the trailing slash
    CREATE OR REPLACE PACKAGE BODY my_package AS
        ...
    BEGIN
        ...
    END;
    /
    
    -- OK since it has the package name after the END
    CREATE OR REPLACE PACKAGE BODY my_package AS
        ...
    BEGIN
        ...
    END my_package;

while this one wouldn't, since it contains an initialization block and it lacks
both the package name after the C<END> and the trailing slash:

    CREATE OR REPLACE PACKAGE BODY my_package AS
        ...
    BEGIN -- initialization block starts here
        ...
    END;

Note however that if the initialization block is absent, the package block will
be correctly isolated even if it lacks both the package name after the C<END>
and the trailing slash.

=back

=head2 Non-limitations

To be split correctly, the given input must, in general, be syntactically valid
SQL. For example, an unbalanced C<BEGIN> or a misspelled keyword could, under
certain circumstances, confuse the parser and make it trip over the next
statement terminator, thus returning wrongly split statements.
This should not be a problem though, as the original (invalid) SQL code would
have been unusable anyway (remember that this is NOT a validating parser!)

=head1 DEPENDENCIES

SQL::SplitStatement depends on the following modules:

=over 4

=item * L<Class::Accessor::Fast>

=item * L<List::MoreUtils>

=item * L<Regexp::Common>

=item * L<SQL::Tokenizer>

=back

=head1 AUTHOR

Emanuele Zeppieri, C<< <emazep@cpan.org> >>

=head1 BUGS

No known bugs.

Please report any bugs or feature requests to
C<bug-sql-SplitStatement at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=SQL-SplitStatement>.
I will be notified, and then you'll automatically be notified of progress
on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc SQL::SplitStatement

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=SQL-SplitStatement>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/SQL-SplitStatement>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/SQL-SplitStatement>

=item * Search CPAN

L<http://search.cpan.org/dist/SQL-SplitStatement/>

=back

=head1 ACKNOWLEDGEMENTS

Igor Sutton for his excellent L<SQL::Tokenizer>, which made writing
this module a joke.

=head1 SEE ALSO

=over 4

=item * L<DBIx::MultiStatementDo>

=item * L<sql-split>

=back

=head1 LICENSE AND COPYRIGHT

Copyright 2010-2011 Emanuele Zeppieri.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation, or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut
