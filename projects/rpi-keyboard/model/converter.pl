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

for my $row (@$json_arr) {
    try {
        my $button_name;
        my $button_hash;
        my $button_count = ( scalar @$row / 2 );

        for my $pos (0..$button_count) {
            say $pos * 2;
        }

        for my $button (@$row) {
            if ( ref($button) eq "HASH" ) {
                $button_hash = $button;
            }
            else {
                $button_name = $button;
            }
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
