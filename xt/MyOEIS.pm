# Copyright 2010, 2011, 2012, 2013 Kevin Ryde

# MyOEIS.pm is shared by several distributions.
#
# MyOEIS.pm is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any later
# version.
#
# MyOEIS.pm is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along
# with this file.  If not, see <http://www.gnu.org/licenses/>.

package MyOEIS;
use strict;
use Carp;
use File::Spec;

# uncomment this to run the ### lines
# use Smart::Comments;

my $without;

sub import {
  shift;
  foreach (@_) {
    if ($_ eq '-without') {
      $without = 1;
    } else {
      die __PACKAGE__." unknown option $_";
    }
  }
}

sub read_values {
  my ($anum, %option) = @_;

  if ($without) {
    return;
  }

  my $seq = eval { require Math::NumSeq::OEIS::File;
                   Math::NumSeq::OEIS::File->new (anum => $anum) };
  my @bvalues;
  my $lo;
  if ($seq) {
    my $count = 0;
    if (($lo, my $value) = $seq->next) {
      push @bvalues, $value;
      while ((undef, $value) = $seq->next) {
        push @bvalues, $value;
      }
    }
  } else {
    my $error = $@;
    @bvalues = __PACKAGE__->stripped_read_values($anum);
    if (! @bvalues) {
      MyTestHelpers::diag ("$anum not available: ", $error);
      return;
    }
  }

  my $desc = "$anum has ".scalar(@bvalues)." values";
  if (@bvalues) { $desc .= " to $bvalues[-1]"; }

  if (my $max_count = $option{'max_count'}) {
    if (@bvalues > $max_count) {
      $#bvalues = $option{'max_count'} - 1;
      $desc .= ", shorten to ".scalar(@bvalues)." values to $bvalues[-1]";
    }
  }

  if (my $max_value = $option{'max_value'}) {
    if ($max_value ne 'unlimited') {
      for (my $i = 0; $i <= $#bvalues; $i++) {
        if ($bvalues[$i] > $max_value) {
          $#bvalues = $i-1;
          if (@bvalues) {
            $desc .= ", shorten to ".scalar(@bvalues)." values to $bvalues[-1]";
          } else {
            $desc .= ", shorten to nothing";
          }
        }
      }
    }
  }
  MyTestHelpers::diag ($desc);

  return (\@bvalues, $lo, $seq->{'filename'});
}

sub oeis_directory {
  # my ($class) = @_;
  require File::Spec;
  require File::HomeDir;
  return File::Spec->catdir(File::HomeDir->home, 'OEIS');
}

sub stripped_filename {
  my ($class) = @_;
  require File::Spec;
  require File::HomeDir;
  return File::Spec->catfile($class->oeis_directory, 'stripped');
}

# return list of values, or empty list if not found
sub stripped_read_values {
  my ($class, $anum) = @_;
  open FH, "< " . __PACKAGE__->stripped_filename
    or return;
  (my $num = $anum) =~ s/^A//;
  my $line = bsearch_textfile (\*FH, sub {
       my ($line) = @_;
       $line =~ /^A(\d+)/ or return -1;
       return ($1 <=> $num);
     })
      || return;
  
  $line =~ s/A\d+ *,?//;
  $line =~ s/\s+$//;
  return split /,/, $line;
}

# with Y reckoned increasing downwards
sub dxdy_to_direction {
  my ($dx, $dy) = @_;
  if ($dx > 0) { return 0; }  # east
  if ($dx < 0) { return 2; }  # west
  if ($dy > 0) { return 1; }  # south
  if ($dy < 0) { return 3; }  # north
}


# Search for a line in text file handle $fh.
# $cmpfunc is called &$cmpfunc($line) and it should do a
# comparison $target <=> $line so
#     0  if $target == $line
#    -ve if $target < $line  so $line is after the target
#    +ve if $target > $line  so $line is before the target
sub bsearch_textfile {
  my ($fh, $cmpfunc) = @_;
  my $lo = 0;
  my $hi = -s $fh;
  for (;;) {
    my $mid = ($lo+$hi)/2;
    seek $fh, $mid, 0
      or last;

    # skip partial line
    defined(readline $fh)
      or last; # EOF

    # position start of line
    $mid = tell($fh);
    if ($mid >= $hi) {
      last;
    }

    my $line = readline $fh;
    defined $line
      or last; # EOF

    my $cmp = &$cmpfunc ($line);
    if ($cmp == 0) {
      return $line;
    }
    if ($cmp < 0) {
      $lo = tell($fh);  # $line is before the target, advance $lo
    } else {
      $hi = $mid;       # $line is after the target, reduce $hi
    }
  }

  seek $fh, $lo, 0;
  while (defined (my $line = readline $fh)) {
    my $cmp = &$cmpfunc($line);
    if ($cmp == 0) {
      return $line;
    }
    if ($cmp > 0) {
      return undef;
    }
  }
  return undef;
}

sub compare_values {
  my %option = @_;
  require MyTestHelpers;
  my $anum = $option{'anum'} || croak "Missing anum parameter";
  my $func = $option{'func'} || croak "Missing func parameter";
  my ($bvalues, $lo, $filename) = MyOEIS::read_values
    ($anum,
     max_count => $option{'max_count'},
     max_value => $option{'max_value'});
  my $diff;
  if ($bvalues) {
    if (my $fixup = $option{'fixup'}) {
      &$fixup($bvalues);
    }
    my ($got,@rest) = &$func(scalar(@$bvalues));
    if (@rest) {
      croak "Oops, func return more than just an arrayref";
    }
    if (ref $got ne 'ARRAY') {
      croak "Oops, func return not an arrayref";
    }
    $diff = diff_nums($got, $bvalues);
    if ($diff) {
      MyTestHelpers::diag ("bvalues: ",join_values($bvalues));
      MyTestHelpers::diag ("got:     ",join_values($got));
    }
  }
  if (defined $Test::TestLevel) {
    require Test;
    local $Test::TestLevel = $Test::TestLevel + 1;
    Test::skip (! $bvalues, $diff, undef, "$anum");
  } elsif (defined $diff) {
    print "$diff\n";
  }
}

sub join_values {
  my ($aref) = @_;
  if (! @$aref) { return ''; }
  my $str = $aref->[0];
  foreach my $i (1 .. $#$aref) {
    my $value = $aref->[$i];
    if (! defined $value) { $value = 'undef'; }
    last if length($str)+1+length($value) >= 275;
    $str .= ',';
    $str .= $value;
  }
  return $str;
}

sub diff_nums {
  my ($gotaref, $wantaref) = @_;
  my $diff;
  for (my $i = 0; $i < @$gotaref; $i++) {
    if ($i > @$wantaref) {
      return "want ends prematurely pos=$i";
    }
    my $got = $gotaref->[$i];
    my $want = $wantaref->[$i];
    if (! defined $got && ! defined $want) {
      next;
    }
    if (defined $got != defined $want) {
      if (defined $diff) {
        return "$diff, and more diff";
      }
      $diff = "different pos=$i got=".(defined $got ? $got : '[undef]')
        ." want=".(defined $want ? $want : '[undef]');
    }
    unless ($got =~ /^[0-9.-]+$/) {
      if (defined $diff) {
        return "$diff, and more diff";
      }
      $diff = "not a number pos=$i got='$got'";
    }
    unless ($want =~ /^[0-9.-]+$/) {
      if (defined $diff) {
        return "$diff, and more diff";
      }
      $diff = "not a number pos=$i want='$want'";
    }
    if ($got != $want) {
      if (defined $diff) {
        return "$diff, and more diff";
      }
      $diff = "different pos=$i numbers got=$got want=$want";
    }
  }
  return $diff;
}

# counting from 1 for prime=2
sub ith_prime {
  my ($i) = @_;
  if ($i < 1) {
    croak "Oops, ith_prime() i=$i";
  }
  require Math::Prime::XS;
  my $to = 100;
  for (;;) {
    my @primes = Math::Prime::XS::primes($to);
    if (@primes >= $i) {
      return $primes[$i-1];
    }
    $to *= 2;
  }
}

sub grep_for_values_aref {
  my ($class, $aref) = @_;
  MyOEIS->grep_for_values(array => $aref);
}
sub grep_for_values {
  my ($class, %h) = @_;
  ### grep_for_values_aref() ...
  ### $class

  my $name = $h{'name'};
  if (defined $name) {
    $name = "$name: ";
  }

  my $values_aref = $h{'array'};
  if (@$values_aref == 0) {
    ### empty ...
    return "${name}no match empty list of values\n\n";
  }

  {
    my $join = $values_aref->[0];
    for (my $i = 1; $i <= $#$values_aref && length($join) < 50; $i++) {
      $join .= ','.$values_aref->[$i];
    }
    $name .= "match $join\n";
  }
  
  if (defined (my $value = constant_array(@$values_aref))) {
    return '';
    if ($value != 0) {
    return "${name}constant $value\n\n";
    }
  }

  if (defined (my $diff = constant_diff(@$values_aref))) {
    return "${name}constant difference $diff\n\n";
  }

  my $values_str = join (',',@$values_aref);

  # print "grep $values_str\n";
  # unless (system 'zgrep', '-F', '-e', $values_str, "$ENV{HOME}/OEIS/stripped.gz") {
  #   print "  match $values_str\n";
  #   print "  $name\n";
  #   print "\n"
  # }
  # unless (system 'fgrep', '-e', $values_str, "$ENV{HOME}/OEIS/oeis-grep.txt") {
  #   print "  match $values_str\n";
  #   print "  $name\n";
  #   print "\n"
  # }
  # unless (system 'fgrep', '-e', $values_str, "$ENV{HOME}/OEIS/stripped") {
  #   print "  match $values_str\n";
  #   print "  $name\n";
  #   print "\n"
  # }
  if (my $str = $class->stripped_grep($values_str)) {
    return "$name$str\n";
  }
}

use constant GREP_MAX_COUNT => 8;
my $stripped_mmap;
sub stripped_grep {
  my ($class, $str) = @_;

  if (! defined $stripped_mmap) {
    require File::HomeDir;
    my $home_dir = File::HomeDir->my_home;
    if (! defined $home_dir) {
      die 'File::HomeDir says you have no home directory';
    }
    my $stripped_filename = File::Spec->catfile($home_dir, 'OEIS', 'stripped');
    require File::Map;
    File::Map::map_file ($stripped_mmap, $stripped_filename);
    print "File::Map stripped file length ",length($stripped_mmap),"\n";
  }

  my $ret = '';
  my $count = 0;

  # my $re = $str;
  # { my $count = ($re =~ s{,}{,(\n|}g);
  #   $re .= ')'x$count;
  # }
  # ### $re

  my $orig_str = $str;
  my $abs = '';
  foreach my $mung ('none', 'negate', 'abs', 'half') {
    if ($ret) { last; }

    if ($mung eq 'none') {

    }  elsif ($mung eq 'negate') {
      $abs = "[NEGATED]\n";
      $str = $orig_str;
      $str =~ s{(^|,)(-?)}{$1.($2?'':'-')}ge;

    } elsif ($mung eq 'half') {
      if ($str =~ /[13579](,|$)/) {
        ### not all even to halve ...
        next;
      }
      $str = join (',', map {$_/2} split /,/, $orig_str);
      $abs = "[HALF]\n";

    } elsif ($mung eq 'abs') {
      $str = $orig_str;
      if (! ($str =~ s/-//g)) {
        ### no negatives to absolute ...
        next;
      }
      if ($str =~ /^(\d+)(,\1)*$/) {
        ### only one value when abs: $1
        next;
      }
      $abs = "[ABSOLUTE VALUES]\n";
    }
    ### $str

    my $pos = 0;
    for (;;) {
      $pos = index($stripped_mmap,$str,$pos);
      last if $pos < 0;

      if ($count >= GREP_MAX_COUNT) {
        $ret .= "... and more matches\n";
        return $ret;
      }

      my $start = rindex($stripped_mmap,"\n",$pos) + 1;
      my $end = index($stripped_mmap,"\n",$pos);
      my $line = substr($stripped_mmap,$start,$end-$start);
      my ($anum) = ($line =~ /^(A\d+)/);
      $anum || die "oops, A-number not matched in line: ",$line;

      $ret .= $abs; $abs = '';
      $ret .= $class->anum_to_name($anum);
      $ret .= "$line\n";

      $pos = $end;
      $count++;
    }
  }
  return $ret;
}

sub anum_to_name {
  my ($class, $anum) = @_;
  $anum =~ /^A[0-9]+$/ or die "Bad A-number: ", $anum;
  return `zgrep -e ^$anum $ENV{HOME}/OEIS/names.gz`;
}

# constant_diff($a,$b,$c,...)
# If all the given values have a constant difference then return that amount.
# Otherwise return undef.
#
sub constant_diff {
  my $diff = shift;
  my $value = shift;
  $diff = $value - $diff;
  while (@_) {
    my $next_value = shift;
    if ($next_value - $value != $diff) {
      return undef;
    }
    $value = $next_value;
  }
  return $diff;
}

# constant_array($a,$b,$c,...)
# If all the given values are all equal then return that value.
# Otherwise return undef.
#
sub constant_array {
  my $value = shift;
  while (@_) {
    my $next_value = shift;
    if ($next_value != $value) {
      return undef;
    }
  }
  return $value;
}

1;
__END__
