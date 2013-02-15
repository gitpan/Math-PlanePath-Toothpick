# Copyright 2012, 2013 Kevin Ryde

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


#
# A151567 four copies of leftist toothpicks
#   becomes 2*left(n)+2*left(n+1)-4n-1  undoubling diagonals
#
# A151565 ,1,1,2,2,2,2, 4, 4, 2, 2,4,4,4,4,8,8,2,2,4,4,4,4,8,8,4,4,8,8,8,8,16,
# A151566 ,0,1,2,4,6,8,10,14,18,20,22,26,30,34,38,46,54,56,58,62,66,70,74,82,90

package Math::PlanePath::ToothpickUpist;
use 5.004;
use strict;

use vars '$VERSION', '@ISA';
$VERSION = 3;
use Math::PlanePath;
@ISA = ('Math::PlanePath');
*_divrem_mutate = \&Math::PlanePath::_divrem_mutate;

use Math::PlanePath::Base::Generic
  'is_infinite',
  'round_nearest';
use Math::PlanePath::Base::Digits
  'round_down_pow',
  'bit_split_lowtohigh',
  'digit_join_lowtohigh';

# uncomment this to run the ### lines
# use Smart::Comments;


use constant default_n_start => 0;
use constant class_x_negative => 1;
use constant class_y_negative => 0;
use constant tree_num_children_maximum => 2;

#------------------------------------------------------------------------------
sub new {
  my $self = shift->SUPER::new(@_);
  if (! defined $self->{'n_start'}) {
    $self->{'n_start'} = $self->default_n_start;
  }
  return $self;
}

sub n_to_xy {
  my ($self, $n) = @_;
  ### ToothpickUpist n_to_xy(): $n

  # written as $n-n_start() rather than "-=" so as to provoke an
  # uninitialized value warning if $n==undef
  $n = $n - $self->{'n_start'};   # N=0 basis

  if ($n < 0) {
    return;
  }
  if ($n == 0 || is_infinite($n)) {
    return ($n,$n);
  }

  # this frac behaviour unspecified yet
  {
    my $int = int($n);
    ### $int
    ### $n
    if ($n != $int) {
      my $frac = $n - $int;  # inherit possible BigFloat
      $int += $self->{'n_start'};
      my ($x1,$y1) = $self->n_to_xy($int);
      my ($x2,$y2) = $self->n_to_xy($int+1);
      my $dx = $x2-$x1;
      my $dy = $y2-$y1;
      return ($frac*$dx + $x1, $frac*$dy + $y1);
    }
    $n = $int;       # BigFloat int() gives BigInt, use that
  }
  ### $n

  my ($depthbits, $lowbit, $ndepth) = _n0_to_depthbits($n);
  ### $depthbits
  ### $ndepth
  ### n remainder: $n-$ndepth

  my @nbits = bit_split_lowtohigh($n-$ndepth); # offset into row

  ### @nbits
  ### $lowbit

  # Where there's a 0-bit in the depth remains a 0-bit.
  # Where there's a 1-bit in the depth takes a bit from Noffset.
  # Small Noffset has less bits than the depth 1s, hence "|| 0".
  #
  my @xbits = map {$_ && (shift @nbits || 0)} @$depthbits;
  ### @xbits

  my $zero = $n * 0;
  my $x = digit_join_lowtohigh (\@xbits,    2, $zero);
  my $y = digit_join_lowtohigh ($depthbits, 2, $zero);

  ### Y without lowbit: $y

  return (2*$x-$y,  # triangular style
          $y + $lowbit);
}

sub xy_to_n {
  my ($self, $x, $y) = @_;
  ### ToothpickUpist xy_to_n(): "$x, $y"

  $y = round_nearest ($y);
  $x = round_nearest($x);

  # odd points X!=Ymod2 are the second copy of the triangle, go to Y-1 for them
  $x += $y;
  my $lowbit = _divrem_mutate ($x, 2);
  $y -= $lowbit;
  ### odd adjusted xy: "$x,$y"

  return _right_xy_to_n ($self, $x,$y, $lowbit);
}

# with X,Y in the align="right" style,
#
#  |
sub _right_xy_to_n {
  my ($self, $x, $y, $lowbit) = @_;
  ### _right_xy_to_n(): "x=$x y=$y lowbit=$lowbit"

  unless ($x >= 0 && $x <= $y && $y >= 0) {
    ### outside horizontal row range ...
    return undef;
  }
  if (is_infinite($y)) {
    return $y;
  }

  my $zero = ($y * 0);
  my $n = $zero;          # inherit bignum 0
  my $npower = $zero+2;   # inherit bignum 2

  my @xbits = bit_split_lowtohigh($x);
  my @depthbits = bit_split_lowtohigh($y);

  my @nbits;  # N offset into row
  foreach my $i (0 .. $#depthbits) {      # x,y bits low to high
    if ($depthbits[$i]) {
      $n = 2*$n + $npower;
      push @nbits, $xbits[$i] || 0;   # low to high
    } else {
      if ($xbits[$i]) {
        return undef;
      }
    }
    $npower *= 3;
  }

  if ($lowbit) {
    push @nbits, 1;
  }

  ### n at left end of y row: $n
  ### n offset for x: @nbits
  ### total: $n + digit_join_lowtohigh(\@nbits,2,$zero) + $self->{'n_start'}

  return $n + digit_join_lowtohigh(\@nbits,2,$zero) + $self->{'n_start'};
}

# not exact
sub rect_to_n_range {
  my ($self, $x1,$y1, $x2,$y2) = @_;
  ### ToothpickUpist rect_to_n_range(): "$x1,$y1, $x2,$y2"

  $y1 = round_nearest ($y1);
  $y2 = round_nearest ($y2);
  if ($y1 > $y2) { ($y1,$y2) = ($y2,$y1) }

  $x1 = round_nearest ($x1);
  $x2 = round_nearest ($x2);
  if ($x1 > $x2) { ($x1,$x2) = ($x2,$x1) }

  if ($y2 < 0) {
    ### all negative ...
    return (1, 0);
  }
  $y1 -= 1;
  if ($y1 < 0) {
    $y1 = 0;
  }

  ### range using: "y1=$y1  y2=$y2"

  return (_right_xy_to_n($self,   0,$y1, 0),
          _right_xy_to_n($self, $y2,$y2, 1));
}

sub tree_n_num_children {
  my ($self, $n) = @_;

  $n = $n - $self->{'n_start'};   # N=0 basis
  if ($n < 0) {
    return undef;
  }
  my ($depthbits, $lowbit, $ndepth) = _n0_to_depthbits($n);
  $n -= $ndepth;  # Noffset into row

  if (! $lowbit) {
    return 1;
  }

  unless (shift @$depthbits) {  # low bit after $lowbit doubling
    # Depth even (or zero), two children under every point.
    return 2;
  }

  # Depth odd, single child under some or all points.
  # When depth==1mod4 it's all points, when depth has more than one
  # trailing 1-bit then it's only some points.
  #
  my $repbit = _divrem_mutate($n,2);
  while (shift @$depthbits) {  # low to high
    if (_divrem_mutate($n,2) != $repbit) {
      return 0;
    }
  }
  return 1;
}

sub tree_n_children {
  my ($self, $n) = @_;
  ### tree_n_children(): $n

  $n = $n - $self->{'n_start'};   # N=0 basis
  if ($n < 0) {
    return;
  }

  my ($depthbits, $lowbit, $ndepth, $nwidth) = _n0_to_depthbits($n);
  if (! $lowbit) {
    ### doubled to children at nwidth below ...
    return ($n + $nwidth);
  }

  $n -= $ndepth;  # Noffset into row

  if (shift @$depthbits) {
    # Depth odd, single child under some or all points.
    # When depth==1mod4 it's all points, when depth has more than one
    # trailing 1-bit then it's only some points.
    while (shift @$depthbits) {  # depth==3mod4 or more low 1s
      my $repbit = _divrem_mutate($n,2);
      if (($n % 2) != $repbit) {
        return;
      }
    }
    return $n + $ndepth+$nwidth + $self->{'n_start'};

  } else {
    # Depth even (or zero), two children under every point.
    $n = 2*$n + $ndepth+$nwidth + $self->{'n_start'};
    return ($n,$n+1);
  }
}

sub tree_n_parent {
  my ($self, $n) = @_;

  my ($x,$y) = $self->n_to_xy($n)
    or return undef;

  if (($x%2) != ($y%2)) {
    ### odd, directly down ...
    return $self->xy_to_n($x,$y-1);
  }

  ### even, to one side or the other ...
  my $n_parent = $self->xy_to_n($x-1, $y);
  if (defined $n_parent) {
    return $n_parent;
  }
  return $self->xy_to_n($x+1,$y);
}

sub tree_n_to_depth {
  my ($self, $n) = @_;
  ### ToothpickUpist n_to_depth(): $n
  $n = $n - $self->{'n_start'};
  if ($n < 0) {
    return undef;
  }
  if (is_infinite($n)) {
    return $n;
  }
  my ($depthbits, $lowbit) = _n0_to_depthbits($n);
  unshift @$depthbits, $lowbit;
  return digit_join_lowtohigh ($depthbits, 2, $n*0);
}
sub tree_depth_to_n {
  my ($self, $depth) = @_;
  ### tree_depth_to_n(): $depth
  if ($depth >= 0) {
    # $depth==+infinity becomes nan from divrem, prefer to return N=+infinity
    # for +inf depth
    if (is_infinite($depth)) {
      return $depth;
    }
    my $lowbit = _divrem_mutate($depth,2);
    return _right_xy_to_n($self,0,$depth, $lowbit);
  } else {
    return undef;
  }
}

# Ndepth = 2 * (        3^a      first N at this depth
#               +   2 * 3^b
#               + 2^2 * 3^c
#               + 2^3 * 3^d
#               + ... )

sub _n0_to_depthbits {
  my ($n) = @_;
  ### _n0_to_depthbits(): $n

  if ($n == 0) {
    return ([], 0, 0, 1);
  }

  my ($nwidth, $bitpos) = round_down_pow ($n/2, 3);
  ### nwidth power-of-3: $nwidth
  ### $bitpos

  $nwidth *= 2;   # two of each row

  my @depthbits;
  my $ndepth = 0;
  for (;;) {
    ### at: "n=$n nwidth=$nwidth bitpos=$bitpos depthbits=".join(',',map{$_||0}@depthbits)

    if ($n >= $ndepth + $nwidth) {
      $depthbits[$bitpos] = 1;
      $ndepth += $nwidth;
      $nwidth *= 2;
    } else {
      $depthbits[$bitpos] = 0;
    }
    last unless --$bitpos >= 0;
    $nwidth /= 3;
  }

  # Nwidth = 2**count1bits(depth)
  ### assert: $nwidth == 2*(1 << scalar(grep{$_}@depthbits))

  # first or second of the two of each row
  $nwidth /= 2;
  my $lowbit = ($n >= $ndepth + $nwidth ? 1 : 0);
  if ($lowbit) {
    $ndepth += $nwidth;
  }
  ### final depthbits: join(',',@depthbits)

  return (\@depthbits, $lowbit, $ndepth, $nwidth);
}

1;
__END__

=for stopwords eg Ryde Sierpinski Nlevel ie Ymin Ymax SierpinskiArrowheadCentres OEIS Online rowpoints Nleft Math-PlanePath-Toothpick Gould's Nend bitand CellularRule Noffset Applegate Automata Congressus Numerantium

=head1 NAME

Math::PlanePath::ToothpickUpist -- self-similar triangular tree traversal

=head1 SYNOPSIS

 use Math::PlanePath::ToothpickUpist;
 my $path = Math::PlanePath::ToothpickUpist->new;
 my ($x, $y) = $path->n_to_xy (123);

=head1 DESCRIPTION

X<Applegate, David>X<Pol, Omar E.>X<Sloane, Neil>This is toothpick variation
where a vertical toothpick may only extend upwards.

=cut

# math-image --path=ToothpickUpist --all --output=numbers --size=180x11

=pod

    66 62    63 67                                  68 64    65 69      10
       58 56 59                                        60 57 61          9
          54 46    47    48    49    50    51    52    53 55             8
             38 34 39    40 35 41    42 36 43    44 37 45                7
                30 26    27 31          32 28    29 33                   6
                   22 20 23                24 21 25                      5
                      18 14    15    16    17 19                         4
                         10  8 11    12  9 13                            3
                             6  4     5  7                               2
                                2  1  3                                  1
                                   0                                <- Y=0

    X= -9 -8 -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8  9 10 ...

It's a 90-degree rotated version of the "leftist" pattern from part 7
"Leftist Toothpicks" of

=over

David Applegate, Omar E. Pol, N.J.A. Sloane, "The Toothpick Sequence and
Other Sequences from Cellular Automata", Congressus Numerantium, volume 206
(2010), pages 157-191.

http://www.research.att.com/~njas/doc/tooth.pdf

=back

As per C<ToothpickTree> (L<Math::PlanePath::ToothpickTree>) each point is
considered a toothpick of length 2, starting from a vertical toothpick at
the origin X=0,Y=0.  Then the pattern grows by adding a toothpick at each
exposed end, so long as it would not cause two toothpicks to overlap (an end
can touch, but they cannot overlap).  The variation here is that vertical
toothpicks can only grow up, so nothing is ever added at the bottom end of a
vertical.

    ...     ...     ...      ...
     |       |       |        |
    10---8--11      12---9---13
     |   |               |    |
         6---4--- ---5---7
         |   |       |   |
             2---1---3
             |   |   |
                 0
                 |

Points are numbered by growth depth and then left to right across the row
within that depth.  This means for example N=6,N=7 are up toothpicks giving
N=8,N=9 in row Y=3, and then those two grow to N=10,N=11 and N=12,N=13
respectively left and right.

=head2 Sierpinski Triangle

X<Sierpinski, Waclaw>As described in the paper above the pattern is a
version of the Sierpinski triangle with each row doubled.  Vertical
toothpicks are on "even" points X==Ymod2 and make the Sierpinski triangle
pattern.  Horizontal toothpicks are on "odd" points X!=Ymod2 and are a
second copy of the triangle, positioned up one at Y+1.

      5                                    h               h
      4     v               v                h   h   h   h
      3       v   v   v   v                    h       h
      2         v       v         plus           h   h
      1           v   v                            h
    Y=0             v

                         gives ToothpickUpist

                     5   ..h..           ..h..
                     4     v h   h   h   h v       
                     3       v h v   v h v
                     2         v h   h v
                     1           v h v
                   Y=0             v

=head1 FUNCTIONS

See L<Math::PlanePath/FUNCTIONS> for behaviour common to all path classes.

=over 4

=item C<$path = Math::PlanePath::ToothpickUpist-E<gt>new ()>

Create and return a new path object.

=back

=head2 Tree Methods

=over

=item C<@n_children = $path-E<gt>tree_n_children($n)>

Return the children of C<$n>, or an empty list if C<$nE<lt>0> (ie. before
the start of the path).

Every vertical toothpick has a single child above it.  The horizontal
toothpicks have either 0, 1 or 2 children according to the Sierpinski
triangle pattern.  (See L<Math::PlanePath::SierpinskiTriangle/N to Number of
Children>).

=item C<$n_parent = $path-E<gt>tree_n_parent($n)>

Return the parent node of C<$n>, or C<undef> if C<$nE<lt>=0> (the start of
tree).

For a horizontal toothpick the parent is the vertical below it.  For a
vertical toothpick the parent is the horizontal to its left or its right,
according to the Sierpinski triangle pattern.

=item C<$depth = $path-E<gt>tree_n_to_depth($n)>

Return the depth of node C<$n>, or C<undef> if there's no point C<$n>.

Each row Y has two depth levels, starting from Y=1 having depth=1 and
depth=2, so depth=ceil(Y/2).

=item C<$n = $path-E<gt>tree_depth_to_n($depth)>

=item C<$n = $path-E<gt>tree_depth_to_n_end($depth)>

Return the first or last N at tree level C<$depth>.  The start of the tree
is depth=0 at the origin X=0,Y=0.

For C<$depth> even this is the N at the left end of each row X=-Y,Y=depth/2.
For C<$depth> odd it's the point above there, 1 in from the left end, so
X=-Y+1,Y=ceil(depth/2).

=back

=head1 OEIS

Entries in Sloane's Online Encyclopedia of Integer Sequences related to this
path include,

    http://oeis.org/A151566    etc

    A151566    total cells at depth=n (tree_depth_to_n())
    A060632    cells added at depth=n (A151565 same)

    A160742    total*2
    A160744    total*3
    A160745    added*3
    A160746    total*4

=head1 SEE ALSO

L<Math::PlanePath>,
L<Math::PlanePath::SierpinskiTriangle>,
L<Math::PlanePath::ToothpickTree>

=head1 HOME PAGE

http://user42.tuxfamily.org/math-planepath/index.html

=head1 LICENSE

Copyright 2012, 2013 Kevin Ryde

Math-PlanePath-Toothpick is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option) any
later version.

Math-PlanePath-Toothpick is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along with
Math-PlanePath-Toothpick.  If not, see <http://www.gnu.org/licenses/>.

=cut

# Local variables:
# compile-command: "math-image --wx --path=ToothpickUpist --all --figure=toothpick --scale=10"
# End:
