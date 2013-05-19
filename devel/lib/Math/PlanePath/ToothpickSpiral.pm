# Copyright 2013 Kevin Ryde

# This file is part of Math-PlanePath-Toothpick.
#
# Math-PlanePath-Toothpick is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# Math-PlanePath-Toothpick is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with Math-PlanePath-Toothpick.  If not, see <http://www.gnu.org/licenses/>.


package Math::PlanePath::ToothpickSpiral;
use 5.004;
use strict;
use Carp;
#use List::Util 'max';
*max = \&Math::PlanePath::_max;

use vars '$VERSION', '@ISA';
$VERSION = 8;
use Math::PlanePath;
@ISA = ('Math::PlanePath');

use Math::PlanePath::Base::Generic
  'is_infinite',
  'round_nearest';

# uncomment this to run the ### lines
# use Smart::Comments;


#          51-50
#           |  |
#             49-48
#                 |
#          19-18 47-46
#           |  |     |
#       21-20 17-16 45-44
#        |        |     |
#    23-22  3--2 15-14 43-42
#     |     |  |     |     |
# 25-24  5--4  1 12-13 40-41
#  |     |        |     |
# 26-27  6--7 10-11 38-39
#     |     |  |     |
#    28-29  8--9 36-37
#        |        |
#       30-31 34-35
#           |  |
#          32-33

# side 2, 6, 10
#      3, 7
#      3, 7
#
# 1,13,41
# N = (8 d^2 + 4 d + 1)
#   = (8*$d**2 + 4*$d + 1)
#   = ((8*$d + 4)*$d + 1)
# d = -1/4 + sqrt(1/8 * $n + -1/16)
#   = (-1 + sqrt(2*$n -1)) / 4

sub n_to_xy {
  my ($self, $n) = @_;
  ### ToothpickSpiral n_to_xy(): $n

  if ($n < 1) { return; }
  if (is_infinite($n)) { return ($n,$n); }

  my $d = int((sqrt(2*$n-1) - 1) / 4);
  $n -= ((8*$d + 4)*$d + 1);
  ### $d
  ### n offset: $n

  my $int = int($n);
  $n -= $int;         # fraction part

  if ($int < 4*$d+2) {
    my ($half, $odd) = _divrem($int, 2);
    if ($odd) {
      return (-$n + 2*$d - $half,       $half+1);
    } else {
      return (      2*$d - $half,  $n + $half);
    }
  }
  $int -= 4*$d+2;

  if ($int < 4*$d+3) {
    my ($half, $odd) = _divrem($int, 2);
    if ($odd) {
      return (-$n - $half - 1,        2*$d - $half);
    } else {
      return (    - $half - 1,  -$n + 2*$d - $half + 1);
    }
  }
  $int -= 4*$d+3;

  if ($int < 4*$d+3) {
    my ($half, $odd) = _divrem($int, 2);
    if ($odd) {
      return (     $half - 2*$d - 1,  -$n - $half - 1);
    } else {
      return ($n + $half - 2*$d - 2,      - $half - 1);
    }
  }
  $int -= 4*$d+3;

  my ($half, $odd) = _divrem($int, 2);
  if ($odd) {
    return (     $half,  $n + $half - 2*$d - 1);
  } else {
    return ($n + $half,       $half - 2*$d - 2);
  }

  return;

}

# return ($quotient, $remainder)
sub _divrem {
  my ($n, $d) = @_;
  if (ref $n && $n->isa('Math::BigInt')) {
    my ($quot,$rem) = $n->copy->bdiv($d);
    if (! ref $d || $d < 1_000_000) {
      $rem = $rem->numify;  # plain remainder if fits
    }
    return ($quot, $rem);
  }
  my $rem = $n % $d;
  return (int(($n-$rem)/$d), # exact division stays in UV
          $rem);
}

sub xy_to_n {
  my ($self, $x, $y) = @_;
  ### ToothpickSpiral xy_to_n(): "$x, $y"

  return undef;
}

# not exact
sub rect_to_n_range {
  my ($self, $x1,$y1, $x2,$y2) = @_;
  ### ToothpickSpiral rect_to_n_range(): "$x1,$y1  $x2,$y2"

  $x1 = round_nearest ($x1);
  $y1 = round_nearest ($y1);
  $x2 = round_nearest ($x2);
  $y2 = round_nearest ($y2);

  return (1, 8 * max(abs($x1),
                     abs($x2),
                     abs($y1),
                     abs($y2)) ** 2);
}

1;
__END__
