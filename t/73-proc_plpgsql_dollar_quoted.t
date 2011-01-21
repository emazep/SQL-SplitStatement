#!/usr/bin/env perl

use strict;
use warnings;

use SQL::SplitStatement;

use Test::More tests => 2;

my $sql_code = <<'SQL';
CREATE LANGUAGE 'plpgsql' HANDLER plpgsql_call_handler
    LANCOMPILER 'PL/pgSQL';

PREPARE some_insert(integer, integer) AS
INSERT  INTO fib_cache (num, fib)
VALUES  ($1, $2);

EXECUTE some_insert(fib_for, ret);

CREATE OR REPLACE FUNCTION fib_fast(
    fib_for integer
) RETURNS integer AS $$
DECLARE
    ret integer := 0;
    nxt integer := 1;
    tmp integer;
BEGIN
    FOR num IN 1..fib_for LOOP
        tmp := ret;
        ret := nxt;
        nxt := tmp + nxt;
    END LOOP;
    RETURN ret;
END;
$$ LANGUAGE plpgsql;

DROP FUNCTION fib_fast(integer);

CREATE FUNCTION somefunc() RETURNS integer AS $$
label
DECLARE
    quantity integer := 30;
BEGIN
    RAISE NOTICE 'Quantity here is %', quantity;  -- Prints 30
    quantity := 50;
    --
    -- Create a subblock
    --
    DECLARE
        quantity integer := 80;
    BEGIN
        RAISE NOTICE 'Quantity here is %', quantity;  -- Prints 80
        RAISE NOTICE 'Outer quantity here is %', outerblock.quantity;  -- Prints 50
    END;

    RAISE NOTICE 'Quantity here is %', quantity;  -- Prints 50

    RETURN quantity;
END label;
$$ LANGUAGE plpgsql;

DROP FUNCTION somefunc(integer);

CREATE FUNCTION funcname (argument-types) RETURNS return-type AS $$
    # PL/Perl function body
$$ LANGUAGE plperl;

SQL

my $splitter;
my @statements;

$splitter = SQL::SplitStatement->new;

@statements = $splitter->split( $sql_code );

cmp_ok(
    @statements, '==', 8,
    'Statements correctly split'
);

$splitter = SQL::SplitStatement->new;
$splitter->keep_extra_spaces(1);
$splitter->keep_empty_statements(1);
$splitter->keep_terminator(1);
$splitter->keep_comments(1);
@statements = $splitter->split( $sql_code );

is(
    join( '', @statements ), $sql_code,
    'SQL code correctly rebuilt'
);
