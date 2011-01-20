#!/usr/bin/env perl

use strict;
use warnings;

use SQL::SplitStatement;

use Test::More tests => 2;

my $original_statements = [
    'CREATE TABLE state (id, "?name?")'                      ,
    'INSERT INTO  state (id, "?name?") VALUES (?, ?)'        ,
    'CREATE TABLE city (id, name, state_id)'                 ,
    'INSERT INTO  city (id, name, state_id) VALUES (?, ?, ?)'
];

my $expected_placeholders = [0, 2, 0, 3];

my $sql_code = <<'SQL';
CREATE TABLE state (id, "?name?");
INSERT INTO  state (id, "?name?") VALUES (?, ?);
-- Comment with question mark?
CREATE TABLE city (id, name, state_id);
INSERT INTO  city (id, name, state_id) VALUES (?, ?, ?)
SQL

my ( $statements, $placeholders );

my $splitter = SQL::SplitStatement->new;

( $statements, $placeholders )
    = $splitter->split_with_placeholders( $sql_code );

is_deeply(
    $statements, $original_statements,
    'Statements correctly split'
);

is_deeply(
    $placeholders, $expected_placeholders,
    'Placeholders correctly calculated'
);
