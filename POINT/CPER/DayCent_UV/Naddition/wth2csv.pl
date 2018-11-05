#!/usr/local/bin/perl

# File: wth2csv.pl
#
# For each .wth file in the current directory, create a .csv (comma delimited) file
# The *.wth files are not modified
#
# Usage: perl wth2csv.pl
#
# Melannie Hartman
# Nov. 27, 2016
#

@wthfiles = glob("*.wth");
foreach $wthfile (@wthfiles)
{
    print $wthfile, "\n";
    $wthfile =~ /(\S+)wth$/;
    $csvfile = $1 . "wth.csv";	# $1 is every character before the "wth" at the end of the filename
    print $csvfile, "\n";
    open(WTHFILE,"$wthfile") || die "ERROR: Unable to open $wthfile\n";
    open(CSVFILE,">$csvfile") || die "ERROR: Unable to open $csvfile\n";
    while ($wthline = <WTHFILE>)
    {
        chomp($wthline);
        $wthline =~ s/^\s+//;      	# Remove whitespace at the beginning of the line
        $wthline =~ s/\s+/,/g;		# Replace remaining whitespace delimiters with a comma
        print CSVFILE "$wthline\n";
    }
    close(WTHFILE);
    close(CSVFILE);
}

