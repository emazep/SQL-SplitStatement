#!/usr/bin/env perl

use strict;
use warnings;

use SQL::SplitStatement;

use Test::More tests => 3;

my $sql_code = <<'SQL';
BEGIN;

CREATE LANGUAGE 'plpgsql' HANDLER plpgsql_call_handler
    LANCOMPILER 'PL/pgSQL';

SAVEPOINT my_savepoint;

PREPARE some_insert(integer, integer) AS
INSERT  INTO fib_cache (num, fib)
VALUES  (?, ?);

ROLLBACK TO my_savepoint;

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

COMMIT;

START TRANSACTION;
DROP FUNCTION fib_fast(integer);
COMMIT;

BEGIN ISOLATION LEVEL SERIALIZABLE;

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

COMMIT;

DROP FUNCTION somefunc(integer);

SQL

my $splitter;
my @statements;
my ($statement, $placeholders);

$splitter = SQL::SplitStatement->new;

@statements = $splitter->split( $sql_code );

cmp_ok(
    @statements, '==', 15,
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

($statement, $placeholders)
    = $splitter->split_with_placeholders( $sql_code );

cmp_ok(
    $placeholders->[3], '==', 2,
    'Statements correctly split'
);