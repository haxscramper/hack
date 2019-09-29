#!/usr/bin/env perl
# -*- coding: utf-8 -*-
# perl

use warnings;
use strict;

# Command line argument parsing
use Pod::Usage;
use Getopt::Long;
use Data::Dumper;
use File::Copy;
use Cwd;

use Text::Template;

# Allow to use true and false as named constants
use constant false => 0;
use constant true  => 1;

# Allow to use functions with parameter signatures
use v5.20;
use feature qw(signatures);
no warnings qw(experimental::signatures);

sub command_exists($comm) {
    return `which $comm 2> /dev/null` ne "";
}

sub write_file($file, $string) {
    open (FH, ">", $file);
    print FH $string;
    close FH;
}

# Pretty message printing
my $pretty_msg = command_exists("colecho");
sub log1($message)  {
    if ($pretty_msg) { system("colecho -- \"$message\"");
    } else { say "  - $message"; } }
sub err1($message)  {
    if ($pretty_msg) { system("colecho -e:2 -- \"$message\"");
    } else { say "!!! $message"; } }
sub info1($message) {
    if ($pretty_msg) { system("colecho -i:1 -- \"$message\"");
    } else { say "--> $message"; } }
sub warn1($message) {
    if ($pretty_msg) { system("colecho -w:1 -- \"$message\"");
    } else { say "=>> $message"; } }


my $quiet_run;
GetOptions(
    "quiet" => \$quiet_run
    );

$quiet_run ||= false;

my $file = "prt_3.java";

my $templ = Text::Template->new(TYPE => 'FILE', SOURCE => 'report_template');

my %vars = (
    files => glob(".$file.tmp.d/*.java"),
);

say $templ->fill_in(HASH => \%vars);

# say for glob(".$file.tmp.d/*.java");
