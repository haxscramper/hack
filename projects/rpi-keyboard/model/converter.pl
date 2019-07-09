#!/usr/bin/env perl

use warnings;
use strict;
use v5.20;
use File::Copy;
use Cwd;
use File::Path qw(make_path remove_tree);
use Switch;

use JSON::MaybeXS qw(encode_json decode_json);

use Pod::Usage;
use Getopt::Long;
use Try::Tiny;

use constant false => 0;
use constant true  => 1;

use feature qw(signatures);
no warnings qw(experimental::signatures);

my $help;
my $in_file;

GetOptions(
    "help"   => \$help,
    "file=s" => \$in_file,
) or pod2usage( -verbose => 1 ) && exit 0;

my $buttons = "";

{
    local $/ = undef;
    open FILE, $in_file or die "Cannot open input file: $! $in_file";
    $buttons = <FILE>;
    close FILE;
}

$buttons = "{\"buttons\":$buttons}";
my $json_arr = ( decode_json $buttons)->{buttons};

my @buttons_;

for my $row (@$json_arr) {
    say "===";
    try {
        my $button_count = ( scalar @$row );
        for my $pos (0..$button_count) {
            my $button_name = $row->[$pos * 2];
            my $button_hash = $row->[$pos * 2 + 1];
            $button_name =~ s/\n*//g;
            say $button_name;
        }
    }
    catch { }
}

=head1 NAME

build.pl

=head1 SYNOPSIS

build.pl [OPTIONS]

=head1 OPTIONS

--help -h  Print help message
--file -f  Input file

=cut
