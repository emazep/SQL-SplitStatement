## no critic
package SQL::SplitStatement::Placeholders;
## use critic

use strict;
use warnings;

use base 'Class::Accessor::Fast';

use constant {
    QUESTION_MARK  => '?',
    SINGLE_DOLLAR  => '$',
    COLON          => ':'
};

#my $name_prefix_RE = qr/[_a-zA-Z]/;
#my $name_tail_RE   = qr/[_a-zA-Z0-9]/;
my $name_RE        = qr/[_a-zA-Z][_a-zA-Z0-9]*/;
my $name_ext_RE    = qr/^[_a-zA-Z][_a-zA-Z0-9]*$/;
my $number_RE      = qr/[0-9]+/;

SQL::SplitStatement->mk_accessors( qw/
    question_mark_params
    dollar_number_params
    dollar_name_params
    colon_number_params
    colon_name_params
    at_number_params
    at_name_params
    _unnamed_params_number
    _named_params
/);

sub new {
    my ($class, $parameters) = @_;
    $class->SUPER::new({
        %$parameters,
        _unnamed_params_number => 0,
        _named_params          => {}
    })
}

sub count_and_reset {
    my $self = shift;
    my $count
        = $self->_unnamed_params_number() + keys %{ $self->_named_params };
    $self->_unnamed_params_number(0);
    $self->_named_params( {} );
    return $count
}

sub _question_mark_param_found {
    my ($self, $token, $next_token) = @_;
    return unless $self->question_mark_params;
    if (
        $token eq QUESTION_MARK && $next_token !~ /^(?:$name_RE|$number_RE)$/
    ) {
        $self->_unnamed_params_number( 1 + $self->_unnamed_params_number );
        return 0
    }
    return
}

sub _dollar_number_param_found {
    my ($self, $token, $next_token) = @_;
    return unless $self->dollar_number_params;
    if ( $token eq SINGLE_DOLLAR && $next_token =~ /^$number_RE$/ ) {
        $self->_named_params->{ $token . $next_token } = 1;
        return 1
    }
    return
}

sub _dollar_name_param_found {
    my ($self, $token, $next_token) = @_;
    return unless $self->dollar_name_params;
    if ( $token eq SINGLE_DOLLAR && $next_token =~ /^$name_RE$/ ) {
        $self->_named_params->{ $token . $next_token } = 1;
        return 1
    }
    return
}

sub _colon_number_param_found {
    my ($self, $token, $next_token) = @_;
    return unless $self->colon_number_params;
    if ( $token =~ /^:$number_RE$/ ) {
        $self->_named_params->{ $token } = 1;
        return 0
    }
    return
}

sub _colon_name_param_found {
    my ($self, $token, $next_token) = @_;
    return unless $self->colon_name_params;
    if ( $token =~ /^:$name_RE$/ ) {
        $self->_named_params->{ $token } = 1;
        return 0
    }
    return
}

sub _at_number_param_found {
    my ($self, $token, $next_token) = @_;
    return unless $self->at_number_params;
    if ( $token =~ /^\@$number_RE$/ ) {
        $self->_named_params->{ $token } = 1;
        return 0
    }
    return
}

sub _at_name_param_found {
    my ($self, $token, $next_token) = @_;
    return unless $self->at_name_params;
    if ( $token =~ /^\@$name_RE$/ ) {
        $self->_named_params->{ $token } = 1;
        return 0
    }
    return
}

sub _is_param {
    my ($self, $token, $next_token) = @_;
    
}


1;

__END__

=head1 NAME

SQL::SplitStatement::Placeholders - Placeholders counting library

=head1 SYNOPSIS

    use SQL::SplitStatement::Placeholders;

=head1 DESCRIPTION

L<SQL::SplitStatement> internal use only: nothing interesting here.

=head1 SEE ALSO

=over 4

=item * L<SQL::SplitStatement>

=item * L<DBIx::MultiStatementDo>

=back

=head1 LICENSE AND COPYRIGHT

Copyright 2010-2011 Emanuele Zeppieri.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation, or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut
