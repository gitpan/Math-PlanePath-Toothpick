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

# uncomment this to run the ### lines
#use Smart::Comments;

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

    if (open FH, "< $ENV{HOME}/OEIS/stripped") {
      (my $num = $anum) =~ s/^A//;
      if (my $line = bsearch_textfile
          (\*FH, sub {
             my ($line) = @_;
             $line =~ /^A(\d+)/ or return -1;
             return ($1 <=> $num);
           })) {
        $line =~ s/A\d+ *,?//;
        $line =~ s/\s+$//;
        @bvalues = split /,/, $line;
      }
    } else {
      MyTestHelpers::diag ("$anum not available: ", $error);
      return;
    }
  }

  my $desc = "$anum has ".scalar(@bvalues)." values to $bvalues[-1]";
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

# with Y reckoned increasing downwards
sub dxdy_to_direction {
  my ($dx, $dy) = @_;
  if ($dx > 0) { return 0; }  # east
  if ($dx < 0) { return 2; }  # west
  if ($dy > 0) { return 1; }  # south
  if ($dy < 0) { return 3; }  # north
}


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
      $lo = tell($fh);  # after
    } else {
      $hi = $mid;
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
  require Test;
  local $Test::TestLevel = $Test::TestLevel + 1;
  Test::skip (! $bvalues, $diff, undef, "$anum");
}

sub join_values {
  my ($aref) = @_;
  if (! @$aref) { return ''; }
  my $str = $aref->[0];
  foreach my $i (1 .. $#$aref) {
    my $value = $aref->[$i];
    if (! defined $value) { $value = 'undef'; }
    last if length($str)+1+length($value) >= 75;
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

1;
__END__
