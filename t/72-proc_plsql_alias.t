#!/usr/bin/env perl

use strict;
use warnings;

use SQL::SplitStatement;

use Test::More tests => 1;

# Bug report (Alexander Sennhauser <as@open.ch>):

my $sql_code = <<'SQL';
CREATE OR REPLACE PACKAGE UTIL IS
   PROCEDURE VERIFY_USER(P_USER_NAME IN VARCHAR2);
END UTIL;
/

CREATE OR REPLACE PACKAGE BODY OS_UTIL IS
   PROCEDURE VERIFY_USER(P_USER_NAME IN VARCHAR2) IS
      a_user varchar2(30);
   BEGIN
      SELECT user INTO a_user FROM dual;
      IF upper(a_user) != upper(p_user_name) THEN
         RAISE_APPLICATION_ERROR(
            -20004,
            'This code can be run as user <' || p_user_name || '> only!'
         );
      END IF;
   END;
END OS_UTIL;
/

CREATE OR REPLACE PACKAGE UTIL IS
   PROCEDURE VERIFY_USER(P_USER_NAME IN VARCHAR2);
END UTIL;
/
SQL

my $splitter;
my @statements;

$splitter = SQL::SplitStatement->new;
@statements = $splitter->split( $sql_code );

#use Data::Dump qw(dump);
#die dump( @statements );

cmp_ok(
    @statements, '==', 3,
    'Statements correctly split'
);

