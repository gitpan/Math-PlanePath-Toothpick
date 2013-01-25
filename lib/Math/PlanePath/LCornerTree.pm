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


package Math::PlanePath::LCornerTree;
use 5.004;
use strict;
#use List::Util 'max';
*max = \&Math::PlanePath::_max;

use vars '$VERSION', '@ISA';
$VERSION = 2;
use Math::PlanePath;
@ISA = ('Math::PlanePath');
*_divrem = \&Math::PlanePath::_divrem;
*_divrem_mutate = \&Math::PlanePath::_divrem_mutate;

use Math::PlanePath::Base::Generic
  'is_infinite',
  'round_nearest';
use Math::PlanePath::Base::Digits
  'round_down_pow',
  'bit_split_lowtohigh',
  'digit_split_lowtohigh',
  'digit_join_lowtohigh';

# uncomment this to run the ### lines
#use Smart::Comments;


use constant default_n_start => 0;
use constant parameter_info_array =>
  [ { name            => 'parts',
      share_key       => 'parts_4to1',
      display         => 'Parts',
      type            => 'integer',
      default         => 4,
      width           => 1,
      minimum         => 1,
      maximum         => 4,
      description     => 'Which parts of the plane to fill, 1 to 4 quadrants.',
    },
    # { name        => 'n_start',
    #   share_key   => 'n_start_0',
    #   type        => 'integer',
    #   default     => 0,
    #   width       => 3,
    #   description => 'Starting N.',
    # },
  ];

sub x_negative {
  my ($self) = @_;
  return ($self->{'parts'} >= 2);
}
sub y_negative {
  my ($self) = @_;
  return ($self->{'parts'} >= 3);
}
use constant tree_num_children_maximum => 3;

sub new {
  my $self = shift->SUPER::new(@_);
  if (! defined $self->{'n_start'}) {
    $self->{'n_start'} = $self->default_n_start;
  }
  $self->{'parts'} ||= 4;
  return $self;
}

my @next_state = (0,12,0,4, 4,0,4,8, 8,4,8,12, 12,8,12,0);
my @digit_to_x = (0,1,1,0, 1,1,0,0, 1,0,0,1, 0,0,1,1);
my @digit_to_y = (0,0,1,1, 0,1,1,0, 1,1,0,0, 1,0,0,1);

sub n_to_xy {
  my ($self, $n) = @_;
  ### LCornerTree n_to_xy(): $n

  if ($n < $self->{'n_start'}) { return; }
  if (is_infinite($n)) { return ($n,$n); }
  {
    my $int = int($n);
    ### $int
    ### $n
    if ($n != $int) {
      my ($x1,$y1) = $self->n_to_xy($int);
      my ($x2,$y2) = $self->n_to_xy($int+1);
      my $frac = $n - $int;  # inherit possible BigFloat
      my $dx = $x2-$x1;
      my $dy = $y2-$y1;
      return ($frac*$dx + $x1, $frac*$dy + $y1);
    }
    $n = $int;       # BigFloat int() gives BigInt, use that
  }

  $n = $n - $self->{'n_start'};  # N=0 basis
  if (is_infinite($n)) { return ($n,$n); }

  my $zero = ($n * 0); # inherit bignum 0
  my $parts = $self->{'parts'};
  my ($depthbits, $ndepth, $nwidth) = _n0_to_depthbits($n, $parts);

  ### $n
  ### $ndepth
  ### $nwidth
  ### $parts
  ### $depthbits

  $n -= $ndepth;  # N remainder offset into row
  ### assert: $n >= 0
  ### assert: $n < $nwidth

  # like a mixed-radix high digit radix $parts then rest radix 3
  $nwidth /= $parts;
  (my $quad, $n) = _divrem($n,$nwidth);
  ### $quad
  ### assert: $quad >= 0
  ### assert: $quad < $parts
  if ($parts == 3) {
    if ($quad == 1) { $quad = 3; } # quad=1 -> 3
    if ($quad == 2) { $quad = 1; } # quad=2 -> 1
  }

  my @nternary = digit_split_lowtohigh($n, 3);
  ### @nternary

  # Ternary digits for triple parts of Noffset mapped out to base4 digits in
  # the style of LCornerReplicate.
  # Where there's a 0-bit in the depth is a 0-digit for Nbase4.
  # Where there's a 1-bit in the depth takes a ternary+1 for Nbase4.
  # Small Noffset has less trits than the depth 1s, hence "nternary || 0".
  #
  my @nbase4 = map {$_ && (1 + (shift @nternary || 0))} @$depthbits;
  ### @nbase4

  my $state = 0;
  my (@xbits, @ybits);
  foreach my $i (reverse 0 .. $#nbase4) {    # digits high to low
    $state += $nbase4[$i];
    $xbits[$i] = $digit_to_x[$state];
    $ybits[$i] = $digit_to_y[$state];
    $state = $next_state[$state];
  }
  my $x = digit_join_lowtohigh (\@xbits, 2, $zero);
  my $y = digit_join_lowtohigh (\@ybits, 2, $zero);

  if ($quad & 1) {
    ($x,$y) = (-1-$y,$x); # rotate +90
  }
  if ($quad & 2) {
    $x = -1-$x; # rotate +180
    $y = -1-$y;
  }
  ### final: "$x,$y"
  return $x,$y;
}

# my @next_state = (0, 1, 3, 2,
# my @yx_to_digit = (0, 1, 3, 2,
#                    0, 1, 3, 2,     # rot +90
#                   );

sub xy_to_n {
  my ($self, $x, $y) = @_;
  ### LCornerTree xy_to_n(): "$x, $y"

  $x = round_nearest ($x);
  $y = round_nearest ($y);

  my $parts = $self->{'parts'};
  my $quad = 0;
  if ($parts == 3) {
    if ($x < 0) {
      if ($y < 0) {
        return undef;
      }
      ($x,$y) = ($y,-1-$x); # rotate -90 and offset
      $quad = 2;
    } else {
      if ($y < 0) {
        ($x,$y) = (-1-$y,$x); # rotate +90 and offset
        $quad = 1;
      }
    }
  } else {
    # parts==1,2,4
    if ($y < 0) {
      if ($parts < 3) {
        return undef;
      }
      $x = -1-$x; # rotate +180
      $y = -1-$y;
      $quad = 2;
    }
    if ($x < 0) {
      if ($parts < 2) {
        return undef;
      }
      ($x,$y) = ($y,-1-$x); # rotate +90 and offset
      $quad++;
    }
  }
  ### $quad
  ### quad rotated xy: "$x,$y"

  if (is_infinite($x)) {
    return $x;
  }
  if (is_infinite($y)) {
    return $y;
  }

  my $zero = ($x * 0 * $y); # inherit bignum 0
  # my @xbits = bit_split_lowtohigh($x);
  # my @ybits = bit_split_lowtohigh($y);
  # my $exp = max($#xbits, $#ybits);
  # my $len = 2**$exp;

  my ($len,$exp) = round_down_pow(max($x,$y), 2);
  my @depthbits;
  my $n = $zero;
  my $ndigits = $zero;

  foreach my $i (reverse 0 .. $exp) {
    ### at: "x=$x,y=$y  n=$n len=$len"

    ### assert: $x >= 0
    ### assert: $y >= 0
    ### assert: $x < 2 * $len
    ### assert: $y < 2 * $len
    ### assert: $len == int($len)

    if ($depthbits[$i] = ($x >= $len || $y >= $len ? 1 : 0)) {
      # one of the three parts away from the origin
      $n *= 3;
      $ndigits++;

      if ($y < $len) {
        ### lower right, digit 0 ...
        ($x,$y) = ($len-1-$y,$x-$len);  # rotate +90 and offset
      } elsif ($x >= $len) {
        ### diagonal, digit 1 ...
        ### right, digit 1 ...
        $x -= $len;
        $y -= $len;
        $n += 1;
      } else {
        ### top left, digit 2 ...
        ($x,$y) = ($y-$len,$len-1-$x);  # rotate -90 and offset
        $n += 2;
      }
    }

    $len /= 2;
  }

  my $depth = digit_join_lowtohigh(\@depthbits,2,$zero);

  ### $n
  ### @depthbits
  ### $depth
  ### $ndigits
  ### npower: 3**$ndigits
  ### $quad
  ### quad powered: $quad*3**$ndigits
  ### result: $n + $quad*3**$ndigits + $self->tree_depth_to_n($depth)

  return $n + $quad*3**$ndigits + $self->tree_depth_to_n($depth);
}

#use Smart::Comments;

# not exact
sub rect_to_n_range {
  my ($self, $x1,$y1, $x2,$y2) = @_;
  ### LCornerTree rect_to_n_range(): "$x1,$y1  $x2,$y2"

  $x1 = round_nearest ($x1);
  $x2 = round_nearest ($x2);
  $y1 = round_nearest ($y1);
  $y2 = round_nearest ($y2);

  ($x1,$x2) = ($x2,$x1) if $x1 > $x2;
  ($y1,$y2) = ($y2,$y1) if $y1 > $y2;

  my $parts = $self->{'parts'};
  if (($parts < 2 && $x2 < 0)
      || ($parts < 3 && $y2 < 0)
      || ($parts == 3 && $x2 < 0 && $y2 < 0))  {
    return (1,0);  # nothing
  }

  my $xymax = max($x2,
                     $y2,
                     ($parts >= 2 ? (-1-$x1) : ()),
                     ($parts >= 3 ? (-1-$y1) : ()));
  ### $xymax
  ($xymax) = round_down_pow($xymax,2);
  ### $xymax
  ### depth_to_n: $self->tree_depth_to_n($xymax+1)
  return ($self->{'n_start'},
          $self->tree_depth_to_n(2*$xymax) - 1);
}

sub tree_depth_to_n {
  my ($self, $depth) = @_;
  ### tree_depth_to_n(): "depth=$depth"

  if (is_infinite($depth)) {
    return $depth;
  }
  unless ($depth >= 0) {
    return undef;
  }
  my $n = ($depth*0);    # bignum 0

  # pow3 = parts * 3^count1bits(depth)
  my $pow3 = $self->{'parts'} + $n; # bignum 1 to 4

  foreach my $bit (reverse bit_split_lowtohigh($depth)) {  # high to low
    $n *= 4;
    if ($bit) {
      $n += $pow3;
      $pow3 *= 3;
    }
  }
  return $n + $self->{'n_start'};
}

sub tree_n_to_depth {
  my ($self, $n) = @_;
  ### LCornerTree n_to_xy(): $n

  if ($n < $self->{'n_start'}) { return undef; }
  $n = int($n) - $self->{'n_start'};  # N=0 basis
  if (is_infinite($n)) {
    return $n;
  }

  my ($depthbits) = _n0_to_depthbits ($n, $self->{'parts'});
  return digit_join_lowtohigh ($depthbits, 2, $n*0);
}

sub _n0_to_depthbits {
  my ($n, $parts) = @_;
  ### _n0_to_depthbits(): $n
  ### $parts

  if ($n < $parts) {
    return ([], 0, $parts);
  }

  my ($nwidth, $bitpos) = round_down_pow ($n/$parts, 4);
  $nwidth *= $parts;
  ### $nwidth
  ### $bitpos

  my @depthbits;
  my $ndepth = 0;
  for (;;) {
    ### at: "n=$n nwidth=$nwidth bitpos=$bitpos depthbits=".join(',',map{$_||0}@depthbits)
    if ($n >= $ndepth + $nwidth) {
      $depthbits[$bitpos] = 1;
      $ndepth += $nwidth;
      $nwidth *= 3;
    } else {
      $depthbits[$bitpos] = 0;
    }
    $bitpos--;
    last unless $bitpos >= 0;
    $nwidth /= 4;
  }

  # Nwidth = 3**count1bits(depth)
  ### @depthbits
  ### assert: $nwidth == $parts * 3 ** (scalar(grep{$_}@depthbits))

  return (\@depthbits, $ndepth, $nwidth);
}

# ENHANCE-ME: step by the bits, not by X,Y
# ENHANCE-ME: tree_n_to_depth() by probe
my @surround_x = (1, 0, -1, 0, 1, -1, 1, -1);
my @surround_y = (0, 1, 0, -1, 1, 1, -1, -1);
sub tree_n_children {
  my ($self, $n) = @_;
  ### LCornerTree tree_n_children(): $n

  if ($n < $self->{'n_start'}) {
    ### before n_start ...
    return;
  }
  my ($x,$y) = $self->n_to_xy($n);
  my @n_children;
  foreach my $i (0 .. 7) {
    if (defined (my $n_surround = $self->xy_to_n($x + $surround_x[$i],
                                                 $y + $surround_y[$i]))) {
      ### $n_surround
      if ($n_surround > $n) {
        my $n_parent = $self->tree_n_parent($n_surround);
        ### $n_parent
        if (defined $n_parent && $n_parent == $n) {
          push @n_children, $n_surround;
        }
      }
    }
  }
  ### @n_children
  # ### assert: scalar(@n_children) == 0 || scalar(@n_children) == 3
  return sort {$a<=>$b} @n_children;
}

sub tree_n_parent {
  my ($self, $n) = @_;
  ### LCornerTree tree_n_parent(): $n

  if ($n < $self->{'n_start'} + $self->{'parts'}) {
    return undef;
  }
  my $want_depth = $self->tree_n_to_depth($n) - 1;
  my ($x,$y) = $self->n_to_xy($n);
  ### $want_depth

  foreach my $i (0 .. 7) {
    if (defined (my $n_surround = $self->xy_to_n($x + $surround_x[$i],
                                                 $y + $surround_y[$i]))) {
      my $depth_surround = $self->tree_n_to_depth($n_surround);
      ### $n_surround
      ### $depth_surround
      if ($depth_surround == $want_depth) {
        return $n_surround;
      }
    }
  }
  ### no parent ...
  return undef;
}

1;
__END__

# sub _NOTWORKING__tree_n_children {
#   my ($self, $n) = @_;
#   my ($power, $exp) = round_down_pow (3*$n-2, 4);
#   $exp -= 1;
#   $power /= 4;
# 
#   ### $power
#   ### $exp
#   ### pow base: 2 + 4*(4**$exp - 1)/3
# 
#   $n -= ($power - 1)/3 * 4 + 2;
#   ### n less pow base: $n
# 
#   my @levelbits = (2**$exp);
#   $power = 3**$exp;
# 
#   # find the cumulative levelpoints total <= $n, being the start of the
#   # level containing $n
#   #
#   my $factor = 4;
#   while (--$exp >= 0) {
#     $power /= 3;
#     my $sub = 4**$exp * $factor;
#     ### $sub
#     # $power*$factor;
#     my $rem = $n - $sub;
# 
#     ### $n
#     ### $power
#     ### $factor
#     ### consider subtract: $sub
#     ### $rem
# 
#     if ($rem >= 0) {
#       $n = $rem;
#       push @levelbits, 2**$exp;
#       $factor *= 3;
#     }
#   }
# 
#   $n += $factor;
#   if (1) {
#     return ($n,$n+1,$n+2);
#   } else {
#     return $n,$n+1,$n+2;
#   }
# }


# n_to_xy() by len.
#
# my $x = $zero;
# my $y = $zero;
# my $len = 1 + $zero;
# my $state = 0;
# my (@xbits, @ybits);
# foreach my $depthbit (reverse @$depthbits) { # low to high
#   my $ndigit = $depthbit && (shift @ndigits || 0);  # low to high
#   ### $ndigit
#
#   if ($ndigit == 0) {
#     ($x,$y) = ($len+$y,$len-1-$x);   # rotate +90
#   } elsif ($ndigit == 1) {
#     $x += $len;
#     $y += $len;
#   } else { # $ndigit == 3
#     ($x,$y) = ($len-1-$y,$len+$x);   # rotate -90
#   }
#   $len *= 2;
# }
# ### assert: @ndigits == 0
#
# if ($quad & 1) {
#   ($x,$y) = (-1-$y,$x); # rotate +90
# }
# if ($quad & 2) {
#   $x = -1-$x; # rotate +180
#   $y = -1-$y;
# }
#
# ### final: "$x,$y"
# return $x,$y;

#          18 17
# 14 13 12 11 16
# 15  6  5 10
#  3  2  4  9
#  0  1  7  8

=for stopwords eg Ryde Math-PlanePath-Toothpick Ulam Warburton Nstart OEIS ie

=head1 NAME

Math::PlanePath::LCornerTree -- cellular automaton growing at exposed corners

=head1 SYNOPSIS

 use Math::PlanePath::LCornerTree;
 my $path = Math::PlanePath::LCornerTree->new;
 my ($x, $y) = $path->n_to_xy (123);

=head1 DESCRIPTION

This is the pattern of a cellular automaton growing by 3 cells from an
exposed corner at each growth level.  Points are numbered anti-clockwise
within their level.  The default is four quadrants starting from four
initial cells N=0 to N=3,

    68  67                          66  65      4
    69  41  40  39  38  35  34  33  32  64      3
        42  20  19  37  36  18  17  31  ...     2
        43  21   8   7   6   5  16  30          1
        44  45   9   1   0   4  28  29     <- Y=0
        47  46  10   2   3  15  63  62         -1
        48  22  11  12  13  14  27  61         -2
        49  23  24  54  55  25  26  60         -3
    70  50  51  52  53  56  57  58  59  75     -4
    71  72                          73  74     -5
                         ^
    -5  -4  -3  -2  -1  X=0  1   2   3   4

The rule is that a cell which is an exposed corner grows by the three cells
surrounding that corner.  So

    depth=0   depth=1         depth=2             depth=3

                                              d d d d d d d d
                                               \| |/   \| |/
                            c c     c c       d-c c-d d-c c-d
                             \|     |/           \|     |/
              b b b b       c-b b b b-c       d-c-b b b b-c-d
               \| |/           \| |/           /|  \| |/  |\
    a a       b-a a-b         b-a a-b         d d b-a a-b d d
         ->             ->               ->
    a a       b-a a-b         b-a a-b         d d b-a a-b d d
               /| |\           /| |\               /| |\  |/
              b b b b       c-b b b b-c       d c-b b b b-c-d
                             /|     |\           /|     |\
                            c c       c       d-c c-d d-c c-d
                                               /| |\   /| |\
                                              d d d d d d d d

"a" is the first cell in each quadrant and grows into the three "b" around
each.  Then for the "b" cells only the corner ones are exposed corners and
they grow to the "c" cells.  Those "c" cells are then all exposed corners
and give a set of 36 "d" cells.  Of those "d"s only the corners are exposed
corners for the next "e" level.

Grouping the three children of each corner shows the growth pattern

=cut

# this image generated by tools/lcorner-tree-bricks.pl

=pod

    +-----------------------+
    |     |     |     |     |
    |  +-----+  |  +-----+  |
    |  |     |  |  |     |  |
    |--|  +-----+-----+  |--|
    |  |  |     |     |  |  |
    |  +--|  +--+--+  |--+  |
    |     |  |  |  |  |     |
    |-----+--+--+--+--+-----|
    |     |  |  |  |  |     |
    |  +--|  +--+--+  |--+  |
    |  |  |     |     |  |  |
    |--|  +-----+-----+  |--|
    |  |     |  |  |     |  |
    |  +-----+  |  +-----+  |
    |     |     |     |     |
    +-----------------------+

In general the number of cells gained in each level is
4*3^count1bits(depth).  So for example depth=3 binary "11" has 2 1-bits so
cells=4*3^2=36.  Through to a depth=2^k adding all those powers-of-3 gives a
power-of-4 square area.

Each side part turns by 90 degrees at its corner, so the plane is filled in
a self-similar style turning into each side quarter, making an attractive
way to fill the plane by a tree structure.  See
L<Math::PlanePath::LCornerReplicate> for a digit-based approach to the
replication.

    +----------------+
    |       |        |
    |  ^    |    ^   |
    |   \   |   /    |
    |    \  |  /     |
    |       |        |
    |-------+--------|
    |       |        |
    |    ^  |  \     |
    |   /   |   \    |
    |  /    |    v   |
    | /     |        |
    +----------------+

=head2 One Quadrant

Option C<parts =E<gt> 1> confines the pattern to the first quadrant.  This
is a single copy of the repeating part in each of the four quadrants of the
full pattern.

=cut

# math-image --path=LCornerTree,parts=1, --all --output=numbers --size=50x5

=pod

    parts => 1

     4  |              18  17
     3  |  14  13  12  11  16
     2  |  15   6   5  10 
     1  |   3   2   4   9 
    Y=0 |   0   1   7   8 
        +---------------------
          X=0   1   2   3   4 

=head2 Half Plane

Option C<parts =E<gt> 2> confines the tree to the upper half plane
C<YE<gt>=0>, giving two symmetric parts above the X axis.

=cut

# math-image --path=LCornerTree,parts=2, --all --output=numbers --size=50x5

=pod

    parts => 2

    36  35                          34  33        4  
    37  27  26  25  24  21  20  19  18  32        3  
        28  12  11  23  22  10   9  17            2  
        29  13   6   5   4   3   8  16            1  
        30  31   7   1   0   2  14  15        <- Y=0 
    --------------------------------------
    -5  -4  -3  -2  -1  X=0  1   2   3   4

=head2 Three Parts

Option C<parts =E<gt> 3> is three replications arranged in a corner down and
left similar to the way the tree grows from a power-of-2 corner X=2^k,Y=2^k.

=cut

# math-image --path=LCornerTree,parts=3, --all --output=numbers --size=50x10

=pod

    parts => 3

    55  54                          50  49        4  
    56  43  42  41  40  28  27  26  25  48        3  
        44  19  18  39  29  14  13  24            2  
        45  20  10   9   5   4  12  23            1  
        46  47  11   2   0   3  21  22        <- Y=0 
    ------------------+  1   8  38  37           -1
                      |  6   7  17  36           -2
                      | 30  15  16  35    
                      | 31  32  33  34  53
                      |             51  52
                         ^
    -5  -4  -3  -2  -1  X=0  1   2   3   4

=head2 Ulam Warburton

Taking just the non-leaf nodes gives the pattern of the Ulam-Warburton
cellular automaton, oriented as per L<Math::PlanePath::UlamWarburtonQuarter>
and using 2x2 blocks.

=cut

# math-image --path=LCornerTree --values=PlanePathCoord,coordinate_type=NumChildren,planepath=LCornerTree  --text --size=30x31
#
# math-image --path=LCornerTree,parts=1 --values='PlanePathCoord,coordinate_type=NumChildren,planepath="LCornerTree,parts=1"' --text --size=17x17

=pod

    parts=>1  non-leaf points
                   ...
     **  **  **  **  
     **  **  **  **  
       **      **    
       **      **    
     **  **  **  **  
     **  **  **  **  
           **        
           **        
     **  **  **  **  
     **  **  **  **  
       **      **    
       **      **    
     **  **  **  **  
     **  **  **  **  
    *

parts=E<gt>4 gives the pattern of L<Math::PlanePath::UlamWarburton>, turned
45 degrees and again in 2x2 blocks.

=pod

=head1 FUNCTIONS

See L<Math::PlanePath/FUNCTIONS> for behaviour common to all path classes.

=over 4

=item C<$path = Math::PlanePath::LCornerTree-E<gt>new ()>

=item C<$path = Math::PlanePath::LCornerTree-E<gt>new (parts =E<gt> $integer)>

Create and return a new path object.  C<parts> can be 1, 2, 3 or 4.

=back

=head2 Tree Methods

=over

=item C<@n_children = $path-E<gt>tree_n_children($n)>

Return the children of C<$n>, or an empty list if C<$n> has no children
(including when C<$n E<lt> 0>, ie. before the start of the path).

X<3-tree>There are either 0 or 3 children.  Such a tree is sometimes called
a "3-tree".  The children of a corner C<$n> are the three cells adjacent to
it which turn to "on" at the next depth.  A non-corner has no children.

=back

=head1 OEIS

This cellular automaton is in Sloane's Online Encyclopedia of Integer
Sequences as

    http://oeis.org/A160410    (etc)

    parts=4 (the default)
      A160410   total cells at given depth (Ndepth)
      A161411   added cells at given depth, 4*3^count1bits(n)

    parts=3
      A160412   total cells at given depth (Ndepth)
      A162349   added cells at given depth, 3*3^count1bits(n)

    parts=1
      A130665   total cells at given depth (Ndepth)
      A048883   added cells at given depth, 3^count1bits(n)

Drawings by Omar Pol

    parts=4
    http://www.polprimos.com/imagenespub/polca023.jpg
    http://www.polprimos.com/imagenespub/polca024.jpg

    parts=3
    http://www.polprimos.com/imagenespub/polca013.jpg
    http://www.polprimos.com/imagenespub/polca027.jpg
    http://www.polprimos.com/imagenespub/polca029.jpg

    parts=1
    http://www.polprimos.com/imagenespub/polca011.jpg
    http://www.polprimos.com/imagenespub/polca012.jpg
    http://www.polprimos.com/imagenespub/polca014.jpg

=head1 SEE ALSO

L<Math::PlanePath>,
L<Math::PlanePath::LCornerReplicate>,
L<Math::PlanePath::UlamWarburton>

=head1 HOME PAGE

http://user42.tuxfamily.org/math-planepath/index.html

=head1 LICENSE

Copyright 2012, 2013 Kevin Ryde

This file is part of Math-PlanePath-Toothpick.

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
