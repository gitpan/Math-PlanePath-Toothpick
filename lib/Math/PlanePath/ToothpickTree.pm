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


# cf A153003  total cells
#    A153004  added cells
#    A153005  total which are primes
#      clipping parts=4 pattern to 3 quadrants,
#      X=0,Y=0 as a half toothpick not counted
#      X=0,Y=-1 as a half toothpick not counted
#      X=1,Y=-1 "root" would begin at depth=1


# A153001 toothpick converge of parts=3 added
#         endless row without exceptions at 2^k points
#
# A160740 toothpick starting from 4 as cross
#   doesn't maintain the XYeven=vertical, XYodd=horizontal
#             |
#             *
#             |
#      ---*--- ---*---
#             |
#             *
#             |
# A160426 cross with one long end for 5 initial toothpicks
# A160730 right angle of 2 toothpicks
# A168112 45-degree something related to 2 toothpick right-angle
# A160732 T of 3 toothpicks
#
# A160158 Toothpick sequence starting from a segment of length 4 formed by two toothpicks.
#      ---*---o---*---
# 
# cf A183004 toothpicks placed at ends, alternately vert,horiz
#    A183148

# cf A160172 T-toothpick sequence
#
# A139250 total cells OFFSET=0 value=0
#    a(2^k) = A007583(k) = (2^(2n+1) + 1)/3
#    a(2^k-1) = A000969(2^k-2), A000969=floor (2*n+3)*(n+1)/3
# A139251 cells added
#   a(2^i)=2^i
#   a(2^i+j) = 2a(j)+a(j+1
#   0, 1, 2,
#   4,  4,
#   4, 8, 12, 8,
#   4, 8, 12, 12, 16, 28, 32, 16,
#   4, 8, 12, 12, 16, 28, 32, 20, 16, 28, 36, 40, 60, 88, 80, 32,
#   4, 8, 12, 12, 16, 28, 32, 20, 16, 28, 36, 40, 60, 88, 80, 36, 16, 28, 36, 40, 60, 88, 84, 56, 60, 92, 112, 140, 208, 256, 192, 64,
#   4, 8, 12, 12, 16, 28, 32, 20, 16, 28

# A160570 triangle, row sums are toothpick cumulative
# A160552 a(2^i+j)=2*a(j)+a(j+1) starting 0,1
# A151548  A160552 row 2^k totals
# A151549   half A151548
# A160762 convolution
#
# cf A160808 count cells Fibonacci spiral
#    A160809 cells added Fibonacci spiral
#
#    A160164 "I"-toothpick
#    A187220 gull

# "Q"
# A187210, A211001-A211003, A211010, A211020-A211024.
# A211011
# A210838 Coordinates (x,y) of the endpoint
# A210841 Coordinates (x,y) of the endpoint
# A211000 Coordinates (x,y) of the endpoint inflection at primes
# http://www.njohnston.ca/2011/03/the-q-toothpick-cellular-automaton/
# maybe hearts A188346 == toothpicks A139250
#
# T(level) = 4 * T(level-1) + 2
# T(level) = 2 * (4^level - 1) / 3
# total = T(level) + 2
# N = (4^level - 1)*2/3
# 4^level - 1 = 3*N/2
# 4^level = 3*N/2 + 1
#
# len=2^level
# total = (len*len-1)*2/3 + 2


#  |     |     |
#  * -*- * -*- *
#  |     |     |
#     |     |
# -*- o -*- * -*-
#     |     |
#  |     |     |
#  * -*- * -*- *
#  |     |     |
#


package Math::PlanePath::ToothpickTree;
use 5.004;
use strict;
#use List::Util 'max','min';
*max = \&Math::PlanePath::_max;
*min = \&Math::PlanePath::_min;

use vars '$VERSION', '@ISA';
$VERSION = 5;
use Math::PlanePath;
@ISA = ('Math::PlanePath');

use Math::PlanePath::Base::Generic
  'is_infinite',
  'round_nearest';
use Math::PlanePath::Base::Digits
  'round_down_pow';

# uncomment this to run the ### lines
# use Smart::Comments;


# Note: some of this shared with ToothpickReplicate
#
use constant n_start => 0;
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
  ];
use constant class_x_negative => 1;
use constant class_y_negative => 1;
sub x_negative {
  my ($self) = @_;
  return ($self->{'parts'} >= 2);
}
sub x_minimum {
  my ($self) = @_;
  return ($self->{'parts'} == 1 ? 1 : undef);
}
sub y_negative {
  my ($self) = @_;
  return ($self->{'parts'} >= 3);
}
sub y_minimum {
  my ($self) = @_;
  return ($self->{'parts'} <= 2 ? 1 : undef);
}
sub rsquared_minimum {
  my ($self) = @_;
  return ($self->{'parts'} == 1   ? 2   # X=1,Y=1
          : $self->{'parts'} == 2 ? 1   # X=0,Y=1
          : 0);
}
use constant tree_num_children_maximum => 2;


# parts=1 Dir4 max 5,-4
#                 14,-9
#                 62,-33
#                126,-65
#            2*2^k-2, -2^k+1   -> 2,-1
#
# parts=2 dX=big,dY=-1 approaches 3.9999
# parts=3 same as parts=1
# parts=4 dX=0,dY=-1 South, apparently
{
  my @dir_maximum_dxdy = (undef,
                          [2,-1],  # 1
                          [0,0],   # 2
                          [2,-1],  # 3
                          [0,-1],  # parts=4
                         );
  sub dir_maximum_dxdy {
    my ($self) = @_;
    return @{$dir_maximum_dxdy[$self->{'parts'}]};
  }
}

#------------------------------------------------------------------------------

sub new {
  my $self = shift->SUPER::new(@_);
  if (! defined $self->{'parts'}) {
    $self->{'parts'} = 4;
  }
  return $self;
}

sub n_to_xy {
  my ($self, $n) = @_;
  ### ToothpickTree n_to_xy(): $n

  if ($n < 0) { return; }
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
  my $zero = $n*0;

  # while ($#{$self->{'n_to_x'}} < $n) {
  #   _extend($self);
  # }
  # ### $self
  #
  # ### x: $self->{'n_to_x'}->[$n]
  # ### y: $self->{'n_to_y'}->[$n]
  # return ($self->{'n_to_x'}->[$n],
  #         $self->{'n_to_y'}->[$n]);



  my $parts = $self->{'parts'};
  if ($parts == 2) {
    if ($n == 0) {
      return (0,1);
    }
  } elsif ($parts == 3) {
    if ($n < 3) {
      if ($n == 0) { return (0,0); }
      if ($n == 1) { return (0,-1); }
      return (0,1); # N==2
    }
  } elsif ($parts == 4) {
    if ($n < 3) {
      if ($n == 0) { return (0,0); }
      if ($n == 1) { return (0,1); }
      return (0,-1); # N==2
    }
  }

  (my $depth, $n) = _n0_to_depth_and_rem($self, $n);
  ### $depth
  ### remainder n: $n

  # $hdx,$hdy is the dx,dy offsets which is "horizontal".  Initially this is
  # hdx=1,hdy=0 so horizontal along the X axis, but subsequent blocks rotate
  # around or mirror to point other directions.
  #
  # $vdx,$vdy is similar dx,dy which is "vertical".  Initially vdx=0,vdy=1
  # so vertical along the Y axis.
  #
  # $mirror is true if in a "mirror image" block.  The difference is that in
  # a plain block points are numbered around anti-clockwise, but when
  # mirrored they're numbered clockwise.
  #
  my $x = 0;
  my $y = 0;
  my $hdx = 1;
  my $hdy = 0;
  my $vdx = 0;
  my $vdy = 1;
  my $mirror = 0;

  if ($parts == 1) {
    $depth += 2;

  } elsif ($parts == 2) {
    $depth += 1;
    my $add = _depth_to_quarter_added([$depth],[1],$zero);
    if ($n >= $add) {
      $n -= $add;
      $hdx = -1; # mirror
      $mirror = 1;
    }
  } elsif ($parts == 3) {
    my $add = _depth_to_quarter_added([$depth+1],[1],$zero);
    if ($n < $add) {
      ### initial part 1, rotate 90 ...
      $depth += 1;
      $x -= 1;
      $hdx = 0;
      $hdy = -1;
      $vdx = 1;
      $vdy = 0;
    } else {
      $n -= $add;
      $add = _depth_to_quarter_added([$depth],[1],$zero);
      if ($n >= $add) {
        ### initial part 3, mirror ...
        $n -= $add;
        $hdx = -1;
        $mirror = 1;
      } else {
        ### initial part 2, unchanged ...
      }
    }

  } elsif ($parts == 4) {
    my $add = _depth_to_quarter_added([$depth],[1],$zero);
    if ($n >= 2*$add) {
      ### initial rotate 180 ...
      $n -= 2*$add;
      $hdx = -1;
      $vdy = -1;
    }
    if ($n >= $add) {
      ### initial mirror ...
      $n -= $add;
      $hdx = -$hdx;
      $mirror = 1;
    }
  }

  ### adjusted to parts4 style depth: "depth=$depth remainder n=$n"

  my ($pow,$exp) = round_down_pow ($depth, 2);
  for ( ; --$exp >= 0; $pow /=2) {
    ### at: "pow=$pow depth=$depth n=$n mirror=$mirror  xy=$x,$y  h=$hdx,$hdy v=$vdx,$vdy"

    if ($depth < $pow) {
      ### part 0 ...
      next;
    }

    $x += $pow/2 * ($hdx + $vdx);
    $y += $pow/2 * ($hdy + $vdy);
    $depth -= $pow;
    ### diagonal to: "depth=$depth  xy=$x,$y"

    if ($depth == 0) {
      ### toothpick A ...
      last;
    }
    if ($depth == 1) {
      ### toothpick other+B ...
      if ($exp && $n == $mirror) {
        ### toothpick other (down): "subtract vdxdy=$vdx,$vdy"
        $x -= $vdx;
        $y -= $vdy;
      } else {
        ### toothpick B (up): "add vdxdy=$vdx,$vdy"
        $x += $vdx;
        $y += $vdy;
      }
      last;
    }

    if ($mirror) {
      #
      # 2 1
      # 3 0
      #
      my $add = _depth_to_quarter_added([$depth],[1],$zero);
      ### add in mirror part1,2: $add

      if ($n < $add) {
        ### mirror part 1, unmirror ...
        $hdx = -$hdx;
        $hdy = -$hdy;
        $mirror = 0;
        next;
      }
      $n -= $add;

      if ($n < $add) {
        ### mirror part 2, same ...
        next;
      }
      $n -= $add;

      ### mirror part 3, rotate -90 ...
      $depth += 1;
      $x -= $hdx; # offset
      $y -= $hdy;
      ($hdx,$hdy, $vdx,$vdy)    # rotate 90 in direction v toward h
        = (-$vdx,-$vdy, $hdx,$hdy);
      ### assert: $n < $add

    } else {
      # not $mirror
      if ($depth+1 < $pow) {
        my $add = _depth_to_quarter_added([$depth+1],[1],$zero);
        ### add in part1: $add

        if ($n < $add) {
          ### part 1, rotate +90 ...
          $depth += 1;
          $x -= $hdx; # offset
          $y -= $hdy;
          ($hdx,$hdy, $vdx,$vdy)    # rotate 90 in direction v toward h
            = (-$vdx,-$vdy, $hdx,$hdy);
          next;
        }
        $n -= $add;
      }

      my $add = _depth_to_quarter_added([$depth],[1],$zero);
      ### add in part2,3: $add

      if ($n < $add) {
        ### part 2, same ...
        next;
      }
      $n -= $add;

      ### part 3, mirror ...
      $hdx = -$hdx;
      $hdy = -$hdy;
      $mirror = 1;
      ### assert: $n < $add
    }
  }

  ### n_to_xy() return: "$x,$y  (depth=$depth n=$n)"
  return ($x,$y);
}

sub xy_to_n {
  my ($self, $x, $y) = @_;
  ### ToothpickTree xy_to_n(): "$x, $y"

  $x = round_nearest ($x);
  $y = round_nearest ($y);

  my $zero = $x * 0 * $y;
  my @add_offset;
  my @add_mult;
  my $mirror = 0;
  my $depth = 0;

  my $parts = $self->{'parts'};
  if ($parts == 2) {
    if ($x == 0) {
      if ($y == 1) { return 0; }
    }
    if ($y == 1) {
      if ($x == 1) { return 1; }
      if ($x == -1) { return 2; }
    }
    $depth = 1;
    if ($x < 0) {
      ### initial mirror second quadrant ...
      $x = -$x;
      $mirror = 1;
      push @add_offset, -1;
      push @add_mult, 1;
    }

  } elsif ($parts == 3) {
    if ($x == 0) {
      if ($y == 0)  { return 0; }
      if ($y == -1) { return 1; }
      if ($y == 1)  { return 2; }
    }
    if ($x >= 0) {
      if ($y < 0) {
        ### initial part 1, rotate and offset ...
        ($x,$y) = (-$y,$x+1);
        $depth = 1;
      } else {
        ### initial part 2, no change ...
        $depth = 2;
        push @add_offset, -1;
        push @add_mult, 1;
      }
    } else {
      if ($y > 0) {
        ### initial part 3, mirror ...
        $mirror = 1;
        $x = -$x;
        push @add_offset, -1;
        push @add_mult, 1;
        push @add_offset, 0;
        push @add_mult, 1;
        $depth = 2;
      } else {
        ### third quad, empty ...
        return undef;
      }
    }

  } elsif ($parts == 4) {
    $depth = 2;
    if ($x == 0) {
      if ($y == 0)  { return 0; }
      if ($y == 1)  { return 1; }
      if ($y == -1) { return 2; }
    }
    if ($x == 1) {
      if ($y == 1)  { return 3; }
      if ($y == -1) { return 6; }
    }
    if ($x == -1) {
      if ($y == 1)  { return 4; }
      if ($y == -1) { return 5; }
    }
    if ($x < 0) {
      $x = -$x;
      push @add_offset, 0;
      if ($y > 0) {
        ### second quad, mirror ...
        push @add_mult, 1;
        $mirror = 1;
      } else {
        ### third quad, rotate ...
        $y = -$y;
        push @add_mult, 2;
      }
    } else {
      if ($y < 0) {
        ### fourth quad, rotate and mirror ...
        $mirror = 1;
        $y = -$y;
        push @add_offset, 0;
        push @add_mult, 3;
      }
    }
  }

  my ($pow,$exp) = round_down_pow (max($x,$y-1), 2);
  $pow *= 2;
  if (is_infinite($exp)) {
    return ($exp);
  }

  my $n = $zero;

  for (;;) {
    ### at: "x=$x,y=$y  pow=$pow depth=$depth mirror=$mirror"

    if ($x == $pow) {
      if ($y == $pow) {
        ### toothpick A, stop ...
        $depth += 2*$pow - 2;
        last;
      }
      if ($y == $pow+1) {
        ### toothpick B, stop ...
        $depth += 2*$pow - 1;
        $n += 1-$mirror;  # "other" first if not mirrored
        last;
      }
      if ($y == $pow-1) {
        ### toothpick other, stop ...
        $depth += 2*$pow - 1;
        $n += $mirror;  # B first if not mirrored
        last;
      }
    }

    if ($y <= $pow) {
      if ($x < $pow) {
        ### part 0, no action ...
      } else {
        ### part 1, rotate and move ...
        $depth += 2*$pow - 1;
        ($x,$y) = ($pow-$y,$x-$pow+1); # shift, rotate +90
        if ($mirror) {
          push @add_offset, $depth-1;  # past part 3,2
          push @add_mult, 2;
        }
      }
    } else {
      ### part 2or3 ...
      $depth += 2*$pow;
      $y -= $pow;
      $x -= $pow;
      if (! $mirror && $y < $pow) {
        push @add_offset, $depth-3;  # past part 1
        push @add_mult, 1;
      }
      if ($x >= 0) {
        ### part 2, same ...
        if ($mirror) {
          push @add_offset, $depth-2;  # when mirrored, past part 3
          push @add_mult, 1;
        }
      } else {
        ### part 3, mirror ...
        if (! $mirror) {
          push @add_offset, $depth-2;  # past part 2
          push @add_mult, 1;
        }
        $x = -$x; # mirror
        $mirror ^= 1;
      }
    }

    if (--$exp < 0) {
      ### final xy: "$x,$y"
      if ($x == 1 && $y == 1) {
      } elsif ($x == 1 && $y == 2) {
        $depth += 1;
      } else {
        ### not in final position ...
        return undef;
      }
      last;
    }
    $pow /= 2;
  }


  ### final depth: $depth
  ### $n
  ### depth_to_n: $self->tree_depth_to_n($depth)
  ### add_offset: join(',',@add_offset)
  ### add_mult:   join(',',@add_mult)

  $n += $self->tree_depth_to_n($depth);

  if (@add_offset) {
    foreach my $add_offset (@add_offset) {
      $add_offset = $depth - $add_offset; # mutate array
      ### add: "depth=$add_offset", _depth_to_quarter_added([$add_offset],[1], $zero)." x "
      # .$add_mult[$i]
    }
    $n += _depth_to_quarter_added (\@add_offset, \@add_mult, $zero);
  }

  # foreach my $i (0 .. $#add_offset) {
  #   $n += (_depth_to_quarter_added ($depth-$add_offset[$i], $zero)
  #          * $add_mult[$i]);
  # }

  ### xy_to_n() return n: $n
  return $n;
}

# Shared with ToothpickReplicate.
# not exact
sub rect_to_n_range {
  my ($self, $x1,$y1, $x2,$y2) = @_;
  ### ToothpickTree rect_to_n_range(): "$x1,$y1  $x2,$y2"

  $x1 = round_nearest ($x1);
  $y1 = round_nearest ($y1);
  $x2 = round_nearest ($x2);
  $y2 = round_nearest ($y2);

  ($x1,$x2) = ($x2,$x1) if $x1 > $x2;
  ($y1,$y2) = ($y2,$y1) if $y1 > $y2;

  my $parts = $self->{'parts'};
  if ($parts == 4) {
    my ($len,$level) = round_down_pow (max(-$x1,
                                           $x2,
                                           -1-$y1,
                                           $y2-1),
                                       2);
    return (0, (32*$len*$len-2)/3);
  }
  if ($parts == 3) {
    if ($x2 < 0 && $y2 < 0) {
      ### third quadrant only, no points ...
      return (1,0);
    }
    # +---------+-------------+
    # | x1,y2-1 | x2,y2-1     |
    # +---------+-------------+
    #           | rot and X-1 |
    #           | x2-1+1,y1   |
    #           +-------------+
    # Point N=28 X=3,Y=-4 and further X=2^k-1,Y=-2^k belong in previous
    # $level level, but don't worry about that for now.
    my ($len,$level) = round_down_pow (max(-$x1,
                                           $x2,
                                           -$y1,
                                           $y2-1),
                                       2);
    return (0, 8*$len*$len);

  }
  if ($parts == 2) {
    if ($y2 < 0) {
      return (1,0);
    }
    my ($len,$level) = round_down_pow (max(-$x1,
                                           $x2,
                                           $y2-1),
                                       2);
    return (0, (16*$len*$len-4)/3);

  }

  ### assert: $parts == 1
  if ($x2 < 1 || $y2 < 1) {
    return (1,0);
  }
  my ($len,$level) = round_down_pow (max($x2, $y2-1),
                                     2);
  return (0, (8*$len*$len-5)/3);
}

# Is it possible to calculate this by the bits of N rather than by X,Y?
sub tree_n_children {
  my ($self, $n) = @_;
  ### tree_n_children(): $n

  my ($x,$y) = $self->n_to_xy($n)
    or return; # before n_start(), no children

  my ($n1,$n2);
  if (($x + $y) % 2) {
    # odd, horizontal to children
    $n1 = $self->xy_to_n($x-1,$y);
    $n2 = $self->xy_to_n($x+1,$y);
  } else  {
    # even, vertical to children
    $n1 = $self->xy_to_n($x,$y-1);
    $n2 = $self->xy_to_n($x,$y+1);
  }
  ### $n1
  ### $n2
  if (($n1||0) > ($n2||0)) {
    ($n1,$n2) = ($n2,$n1); # sorted
  }
  return ((defined $n1 && $n1 > $n ? $n1 : ()),
          (defined $n2 && $n2 > $n ? $n2 : ()));
}

sub tree_n_parent {
  my ($self, $n) = @_;
  ### tree_n_parent(): $n

  $n = int($n);
  if ($n < 1) {
    return undef;
  }
  my ($x,$y) = $self->n_to_xy($n)
    or return undef;

  ### parent at: "xy=$x,$y"
  ### parent odd  list: (($x%2) ^ ($y%2))  && ($self->xy_to_n_list($x,$y-1), $self->xy_to_n_list($x,$y+1))
  ### parent even list: !(($x%2) ^ ($y%2)) && ($self->xy_to_n_list($x-1,$y), $self->xy_to_n_list($x+1,$y))
  ### parent min: min($self->xy_to_n_list($x-1,$y), $self->xy_to_n_list($x+1,$y),$self->xy_to_n_list($x,$y-1), $self->xy_to_n_list($x,$y+1))

  return min((($x%2) ^ ($y%2))
             ?
             # odd X,Y, vertical to parent
             ($self->xy_to_n_list($x,$y-1),
              $self->xy_to_n_list($x,$y+1))
             :
             # even X,Y, horizontal to parent
             ($self->xy_to_n_list($x-1,$y),
              $self->xy_to_n_list($x+1,$y)));
}

sub tree_n_to_depth {
  my ($self, $n) = @_;
  ### tree_n_to_depth(): "$n"

  if ($n < 0) {
    return undef;
  }
  my ($depth) = _n0_to_depth_and_rem($self, int($n));
  ### n0 depth: $depth
  return $depth;
}

sub tree_n_to_height {
  my ($self, $n) = @_;
  if (is_infinite($n)) {
    return $n;
  }
  {
    # infinite height on X=Y, X=Y-1 spines
    my ($x,$y) = $self->n_to_xy($n);
    $x = abs($x);
    $y = abs($y);
    if ($x == $y || $x == $y-1) {
      return undef;
    }
  }
  my @n = ($n);
  my $height = 0;
  for (;;) {
    @n = map {$self->tree_n_children($_)} @n
      or return $height;
    $height++;
  }
}

# Do a binary search for the bits of depth which give Ndepth <= N.
#
# Ndepth grows as roughly depth*depth, so this is about log4(N) many
# compares.  For large N wouldn't want to a table to search through to
# sqrt(N).

sub _n0_to_depth_and_rem {
  my ($self, $n) = @_;
  ### _n0_to_depth_and_rem(): "n=$n   parts=$self->{'parts'}"

  # For parts=4 have depth=2^exp formula
  # T[2^exp] = parts*(4^exp-1)*2/3 + 3
  # parts*(4^exp-1)*2/3 + 3 = N
  # 4^exp = (N-3)*3/2parts,   round down
  # but must be bigger ... (WHY-IS-IT-SO?)
  #
  my ($pow,$exp) = round_down_pow (6*$n/$self->{'parts'},
                                   4);
  if (is_infinite($exp)) {
    return ($exp,0);
  }
  ### $pow
  ### $exp

  my $depth = 0;
  my $n_depth = 0;
  $pow = 2 ** $exp;  # pow=2^exp down to 1, inclusive

  while ($exp-- >= 0) {
    my $try_depth = $depth + $pow;
    my $try_n_depth = $self->tree_depth_to_n($try_depth);

    ### $depth
    ### $pow
    ### $try_depth
    ### $try_n_depth

    if ($try_n_depth <= $n) {
      ### use this tried depth ...
      $depth = $try_depth;
      $n_depth = $try_n_depth;
    }
    $pow /= 2;
  }

  ### _n0_to_depth_and_rem() final ...
  ### $depth
  ### remainder: $n - $n_depth

  return ($depth, $n - $n_depth);
}

# First unsorted @pending
#   depth=119 parts=4 pow=64   119
#   depth=119 parts=4 pow=32   56,55
#   depth=119 parts=4 pow=16   25,24,23
#   depth=119 parts=4 pow=8    10,9,8,7     <- list crosses pow=8 boundary
#   depth=119 parts=4 pow=4    3,2,7
#   depth=119 parts=4 pow=2    3

# T(2^k+rem) = T(2^k) + T(rem) + 2T(rem-1)   rem>=1
#
my @parts_depth_to_n_offset = (undef, 0,1,2,3);
my @parts_depth_adjust = (undef, 2,1,1,0);  # to give parts=4 style numbering

sub tree_depth_to_n {
  my ($self, $depth) = @_;
  ### tree_depth_to_n(): "$depth  parts=$self->{'parts'}"

  if ($depth < 0) {
    return undef;
  }
  $depth = int($depth);
  if ($depth < 2) {
    return $depth;  # 0,1, for any $parts
  }

  my $parts = $self->{'parts'};

  # Adjust $depth so it's parts=4 style counting from the origin X=0,Y=0 as
  # depth=0.  So for example parts=1 is adjusted $depth+=2 since its depth=0
  # is at X=1,Y=1 which is 2 levels down.
  #
  # The parts=4 style means that depth=2^k is the "A" point of a new
  # replication.
  #
  $depth += $parts_depth_adjust[$parts];

  my ($pow,$exp) = round_down_pow ($depth, 2);
  if (is_infinite($exp)) {
    return $exp;
  }
  ### $pow
  ### $exp

  # initial toothpicks not counted by the blocks crunching
  #   parts=1   0
  #   parts=2   1 middle X=0,Y=1
  #   parts=3   2 = 1 origin + 1 above
  #   parts=4   3 = 1 origin + 1 above + 1 below
  # hence begin N=parts-1
  #
  my $zero = $depth*0;
  my $n = $parts-1 + $zero;

  # @pending is a list of depth values.
  # @mult is the multiple of T[depth] desired for that @pending entry.
  #
  # @pending is mostly high to low and growing by one more value at each
  # $exp level, but sometimes it's a bit more and some values not high to
  # low and possibly duplicated.
  #
  my @pending;
  my @mult;
  if ($parts == 3) {
    @pending = ($depth, $depth-1);
    @mult = (1+$zero, 2+$zero);
  } else {
    @pending = ($depth);
    @mult = ($parts+$zero);
  }

  while (--$exp >= 0) {
    last unless @pending;

    ### @pending
    ### @mult
    ### $exp
    ### $pow

    my @new_pending;
    my @new_mult;
    my $tpow;

    # if (1||join(',',@pending) ne join(',',reverse sort {$a<=>$b} @pending)) {
    #   # print "depth=$depth parts=$parts pow=$pow   ",join(',',@pending),"\n";
    #   print "mult  ",join(',',@mult),"\n";
    # }

    foreach my $depth (@pending) {
      my $mult = shift @mult;
      ### assert: $depth >= 2
      ### assert: $depth < 2*$pow

      if ($depth <= 3) {
        if ($depth == 3) {
          ### depth==3 total=1 ...
          $n += $mult;
        } else {
          ### depth==2 total=0 ...
        }
        next;
      }

      if ($depth < $pow) {
        # Smaller than $pow, keep unchanged.  Cannot stop processing
        # @pending on finding one $depth<$pow because @pending is not quite
        # sorted and therefore might have a later $depth>=$pow.
        push @new_pending, $depth;
        push @new_mult, $mult;
        next;
      }
      my $rem = $depth - $pow;

      ### $depth
      ### $mult
      ### $rem
      ### assert: $rem >= 0 && $rem < $pow

      my $basemult = $mult;  # multiple of T(2^k) base part

      if ($rem == 0) {
        ### rem==0, so just the T(2^k) part ...

      } elsif ($rem == 1) {
        ### rem==1 "A" ...
        $n += $mult;

      } else {
        ### rem >= 2, formula ...
        # T(pow+rem) = T(pow) + T(rem+1) + 2T(rem) + 2
        $n += 2*$mult;

        $rem += 1;   # for rem+1
        if ($rem == $pow) {
          ### rem==pow-1 so rem+1==pow is an extra T(2^k) ...
          $basemult += $mult;
        } elsif (@new_pending && $new_pending[-1] == $rem) {
          ### combine rem+1 here with rem of previous ...
          $new_mult[-1] += $mult;
        } else {
          push @new_pending, $rem;
          push @new_mult, $mult;
        }
        if ($rem -= 1) {  # for rem
          push @new_pending, $rem;
          push @new_mult, 2*$mult;
        }
      }

      # T(2^k) = (4^(k-1)-1)*2/3 = (pow*pow-4)/6
      $tpow ||= ($pow*$pow-4)/6;
      $n += $basemult * $tpow;
    }
    @pending = @new_pending;
    @mult = @new_mult;
    $pow /= 2;
  }

  ### return: $n
  return $n;
}


# $depth numbered from origin in parts=4 style.
# Return added at that depth, ie. depth_to_n($depth+1)-depth_to_n($depth)
#
# @$depth_list is a list of depth values.
# @mult_list is the multiple of T[depth] desired for that @$depth_list entry.
#
# @$depth_list is maintained mostly high to low and growing by one more
# value at each $exp level, but sometimes it's a bit more and some values
# not high to low and possibly duplicated.
#
# my @$depth_list = ($depth);
# my @mult_list = (1 + $zero);

sub _depth_to_quarter_added {
  my ($depth_list, $mult_list, $zero) = @_;
  ### _depth_to_quarter_added(): join(',',@$depth_list)
  ### assert: scalar(@$depth_list) >= 1
  ### assert: max(@$depth_list) == $depth_list->[0]

  my ($pow,$exp) = round_down_pow ($depth_list->[0], 2);
  if (is_infinite($exp)) {
    return $exp;
  }
  ### $pow
  ### $exp

  my $add = $zero;

  while (--$exp >= 0) {     # running $pow down to 2 (inclusive)
    ### assert: $pow >= 2
    last unless @$depth_list;

    ### pending: join(',',@$depth_list)
    ### mult   : join(',',@$mult_list)
    ### $exp
    ### $pow

    my @new_depth_list;
    my @new_mult_list;

    foreach my $depth (@$depth_list) {
      ### assert: $depth >= 2
      ### assert: $depth == int($depth)

      my $mult = shift @$mult_list;
      ### assert: $depth >= 1

      if ($depth <= 3) {
        ### depth==2or3 add=1 ...
        $add += $mult;
        next;
      }

      if ($depth < $pow) {
        # less than 2^exp so unchanged
        push @new_depth_list, $depth;
        push @new_mult_list, $mult;
        next;
      }

      my $rem = $depth - $pow;

      ### $depth
      ### $mult
      ### $rem
      ### assert: $rem >= 0 && $rem <= $pow

      if ($rem == 0 || $rem == $pow) {
        ### rem==0, A of each, add=1 ...
        $add += $mult;

      } elsif ($rem == 1) {
        ### rem==1, B and other, add=2 ...
        $add += 2*$mult;

      } elsif ($rem == $pow-1) {
        ### depth=2*pow-1, add=pow ...
        # formula would be A(pow+rem) = 2*A(rem)
        $add += $mult*$pow/2;

      } else {
        ### rem >= 2, formula ...
        # A(pow+rem) = A(rem+1) + 2A(rem)

        $rem += 1;
        if (@new_depth_list && $new_depth_list[-1] == $rem) {
          # add to previously pushed pending depth
          # print "rem=$rem ",join(',',@new_depth_list),"\n";
          $new_mult_list[-1] += $mult;
        } else {
          push @new_depth_list, $rem;
          push @new_mult_list, $mult;
        }
        push @new_depth_list, $rem-1;
        push @new_mult_list, 2*$mult;
      }
    }
    $depth_list = \@new_depth_list;
    $mult_list  = \@new_mult_list;
    $pow /= 2;
  }

  ### return: $add
  return $add;
}

1;
__END__

# Bigger sample of parts=2 ...
#
#    --37--    36--                  --35--  --34--      6
#       |       |                       |       |
#      31--25--30                      29--24--28        5
#           |                               |
#          22--20--  --19--  --18--  --17--21            4
#           |   |       |       |       |   |
#      32--26- 15---9--14      13---8--12 -23--27        3
#       |       |   |               |   |       |
#    --38--         6---4--  ---3---5        --33--      2
#               |   |   |       |   |   |
#              16--10-  2---0---1 --7--11                1
#               |       |       |       |
#                                                   <- Y=0
#    ---------------------------------------------
#                           ^
#      -5  -4  -3  -2  -1  X=0  1   2   3   4   6


=for stopwords eg Ryde Math-PlanePath-Toothpick Applegate Automata Congressus Numerantium OEIS ie Ndepth Nquad

=head1 NAME

Math::PlanePath::ToothpickTree -- toothpick pattern by growth levels

=head1 SYNOPSIS

 use Math::PlanePath::ToothpickTree;
 my $path = Math::PlanePath::ToothpickTree->new;
 my ($x, $y) = $path->n_to_xy (123);

=head1 DESCRIPTION

X<Applegate, David>X<Pol, Omar E.>X<Sloane, Neil>This is the "toothpick"
sequence expanding through the plane by non-overlapping line segments as per

=over

David Applegate, Omar E. Pol, N.J.A. Sloane, "The Toothpick Sequence and
Other Sequences from Cellular Automata", Congressus Numerantium, volume 206
(2010), 157-191

http://www.research.att.com/~njas/doc/tooth.pdf

=back

Points are numbered by growth levels and anti-clockwise around within the
level.

=cut

# math-image --path=ToothpickTree --output=numbers --all --size=65x11

=pod

    --49---                         --48---            5
       |                               |
      44--38--  --37--  --36--  --35--43               4
       |   |       |       |       |   |
    --50- 27--17--26      25--16--24 -47---            3
           |   |               |   |
              12---8--- ---7--11                       2
           |   |   |       |   |   |
          28--18-- 4---1---3 -15--23                   1
           |       |   |   |       |
                       0                          <- Y=0
           |       |   |   |       |
          29--19-  5---2---6 -22--34                  -1
           |   |   |       |   |   |
              13---9--  --10--14                      -2
           |   |   |       |   |   |
    --51- 30--20--31      32--21--33 -54---           -3
       |   |       |       |       |   |
      45--39--- --40--- --41--- --42--46              -4
       |                               |
    --52---                         --53---           -5

                       ^
      -4   -3 -2  -1  X=0  1   2   3   4

Each X,Y is the centre of a toothpick of length 2.  The first toothpick is
vertical at the origin X=0,Y=0.

A toothpick is added at each exposed end, perpendicular to that end.  So N=1
and N=2 are added to the two ends of the initial N=0 toothpick.  Then points
N=3,4,5,6 are added at the four ends of those.

                                               ---8--- ---7---
                              |       |           |       |
             ---1---          4---1---3           4---1---3
    |           |             |   |   |           |   |   |
    0   ->      0       ->        0        ->         0
    |           |             |   |   |           |   |   |
             ---2---          5---2---6           5---2---6
                              |       |           |       |
                                               ---9--- --10---

Toothpicks are not added if they would overlap.  This means no toothpick at
X=1,Y=0 where the ends of N=3 and N=6 meet, and likewise not at X=-1,Y=0
where N=4 and N=5 meet.

The end of a new toothpick is allowed to touch an existing toothpick.  The
first time this happens is N=15 where its left end touches N=3.

The way each toothpick is perpendicular to the previous means that at even
depth the toothpicks are all vertical and on "even" points X==Y mod 2.
Conversely at odd depth all toothpicks are horizontal and on "odd" points
X!=Y mod 2.  (The initial N=0 is depth=0.)

The children at a given depth are numbered in order of their parents, and
anti-clockwise around when there's two children.

            |       |
            4---1---3         points 3,4 numbered
            |   |   |         anti-clockwise around
                0
                |

Anti-clockwise here is relative to the direction of the grandparent node.
So for example at N=1 its parent N=0 is downwards and the children of N=1
are then anti-clockwise around from there, hence first the right side for
N=3 and then the left for N=4.

=head2 Cellular Automaton

The toothpick rule can also be expressed as growing into a cell which has
just one of its two vertical or horizontal neighbours "ON", using either
vertical or horizontal neighbours according to X+Y odd or even.

          Point            Grow
    ------------------   ------------------------------------------
    "even", X==Y mod 2   turn ON if 1 of 2 horizontal neighbours ON
    "odd",  X!=Y mod 2    turn ON if 1 of 2 vertical neighbours ON

For example X=0,Y=1 which is N=1 turns ON because it has a single vertical
neighbour (the origin X=0,Y=0).  But the cell X=1,Y=0 never turns ON because
initially its two vertical neighbours are OFF and then later at depth=3
they're both ON.  Only when there's exactly one of the two neighbours ON in
the relevant direction does the cell turn ON.

In the paper section 10 above this variation between odd and even points is
reckoned as an automaton on a directed graph where even X,Y points have
edges directed out horizontally, and conversely odd X,Y points are directed
out vertically.

         v          ^         v          ^         v
    <- -2,2  ---> -1,2  <--- 0,2  --->  1,2 <---  2,2 --
         ^          |         ^          |         ^
         |          v         |          v         |
    -> -2,1  <--- -1,1  ---> 0,1  <---  1,1 --->  2,1 <-
         |          ^         |          ^         |
         v          |         v          |         v
    <- -2,0  ---> -1,0  <--- 0,0  --->  1,0 <---  2,0 ->
         ^          |         ^          |         ^
         |          v         |          v         |
    -> -2,-1 <--- -1,-1 ---> 0,1  <--- 1,-1 ---> 2,-1 <-
         |          ^         |          ^         |
         v          |         v          |         v
    <- -2,-2 ---> -1,-2 <--- 0,-2 ---> 1,-2 <--- 2,-2 ->
         ^          v         ^          v         ^

The rule on this graphis then that a cell turns ON if precisely one of it's
neighbours is ON, looking along the outward directed edges.  For example
X=0,Y=0 starts as ON then the cell above X=0,Y=1 considers its two
outward-edge neighbours 0,0 and 0,2, of which just 0,0 is ON and so 0,1
turns ON.

=head2 Replication

Within each quadrant the pattern repeats in blocks of a power-of-2 size,
with an extra two toothpicks "A" and "B" in the middle.

    |
    |------------+------------A
    |            |            |
    |  block 3       block 2  |      in each quadrant
    |   mirror        same    |
    |     ^            ^      |
    |      \   --B--  /       |
    |       \    |   /        |
    |----------  A         ---+
    |            |            |
    |  block 0       block 1  |
    |     ^      |  \ rot +90 |
    |    /       |   \        |
    |   /        |    v       |
    +----------------------------

Toothpick "A" is at a power-of-2 position X=2^k,Y=2^k and toothpick "B" is
above it.  The B toothpick leading to blocks 2 and 3 means block 1 is one
growth level ahead of blocks 2 and 3.

In the first quadrant of the diagram above, N=3,N=7 is block 0 and those two
repeat as N=15,N=23 block 1, and N=24,N=35 block 2, and N=25,36 block 3.
The rotation for block 1 can be seen.  The mirroring for block 3 can be seen
at the next level (the diagram of the L</One Quadrant> form below extends to
there).

The initial N=3,N=7 can be thought of as an "A,B" middle pair with empty
blocks before and surrounding.

See L<Math::PlanePath::ToothpickReplicate> for a digit-based replication
instead of by growth levels.

=head2 Level Ranges

Each "A" toothpick is at a power-of-2 position,

   "A" toothpick
   -------------
   X=2^k, Y=2^k
   depth = 4^k              counting from depth=0 at the origin
   N = (8*4^k + 1)/3        N=3,11,43, etc
     = 222...223 in base4

N=222..223 in base-4 arises from the replication described above.  Each
replication is 4*N+2 of the previous, after the initial N=0,1,2.

The "A" toothpick coming out of corner of block 2 is the only growth from a
depth=4^k level.  The sides of blocks 1 and 2 and blocks 2 and 3 have all
endpoints meeting and so stop by the no-overlap rule, as can be seen for
example N=35,36,37,38 across the top above.

The number of points visited approaches 2/3 of the plane.  This be seen by
expressing the count of points up to "A" as a fraction of the area (in all
four quadrants) to there,

    N to "A"   (8*4^k + 1)/3      8/3 * 4^k
    -------- = -------------   -> --------- = 2/3
    Area X*Y   (2*2^k)*(2*2^k)    4   * 4^k

=head2 One Quadrant

Option C<parts =E<gt> 1> confines the pattern to the first quadrant,
starting from N=0 at X=1,Y=1 which is the first toothpick wholly within that
first quadrant.  This is a single copy of the repeating part in each of the
four quadrants of the full pattern.

=cut

# math-image --path=ToothpickTree,parts=1 --all --output=numbers

=pod

    parts => 1
                                    ...     ...
        |                            |       |
        |                           47--44--46
        |                                |   |
      8 | --41--  --40--  --39--  --38--42
        |    |       |       |       |   |   |
      7 |   36--28--35      34--27--33 -43--45
        |    |   |               |   |       |
      6 |       22--18--  --17--21          ...
        |        |   |       |   |   |
      5 | --37--29- 15--12--14 -26--32
        |                |   |       |
      4 | ---9--- ---8--10
        |    |       |   |   |       |
      3 |    7---4---6 -11--13 -25--31
        |        |   |       |   |   |
      2 | ---1---2      19--16--20
        |    |   |   |   |       |   |
      1 |    0 --3---5  23    --24--30
        |    |       |               |
    Y=0 |
        +----------------------------------
        X=0  1   2   3   4   5   6   7   8

The "A" toothpick at X=2^k,Y=2^k is

    N of "A" = (2*4^k - 2)/3 = 2,10,42,etc
             = "222...222" in base 4

The repeating part starts from N=0 here so there's no initial centre
toothpicks like the full pattern.  This means the repetition is a plain
4*N+2 and hence a N="222..222" in base 4.  It also means the depth is 2
smaller, since N=0 depth=0 at X=1,Y=1 corresponds to depth=2 in the full
pattern.

=head2 Half Plane

Option C<parts =E<gt> 2> confines the tree to the upper half plane
C<YE<gt>=1>, giving two symmetric parts above the X axis.  N=0 at X=0,Y=1 is
the first toothpick of the full pattern which is wholly within this half
plane.

=cut

# math-image --path=ToothpickTree,parts=2 --all --output=numbers

=pod

    parts => 2

    ...                             ...           5
     |                               |
    22--20--  --19--  --18--  --17--21            4
     |   |       |       |       |   |
    ... 15---9--14      13---8--12  ...           3
         |   |               |   |
             6---4--  ---3---5                    2
         |   |   |       |   |   |
        16--10-  2---0---1 --7--11                1
         |       |       |       |
                                             <- Y=0
    -----------------------------------
                     ^
    -4  -3  -2  -1  X=0  1   2   3   4

=head2 Three Parts

Option C<parts =E<gt> 3> is the three replications which occur from an
X=2^k,Y=2^k point, continued on indefinitely confined to the upper and right
three quadrants.

=cut

# math-image --path=ToothpickTree,parts=3 --all --output=numbers

=pod

    parts => 3

    ..--32--  --31--  --30--  --29--..           4
         |       |       |       |
        26--18--25      24--17--23               3
         |   |               |   |
            12---8--  ---7--11                   2
         |   |   |       |   |   |
        27--19-  5---2---4 -16--22               1
         |       |   |   |       |
                     0                      <- Y=0
                     |   |       |
                  ---1---3 -15--21              -1
                         |   |   |
                     9---6--10                  -2
                     |       |   |
                  --13--  --14--20              -3
                                 |
                            ..--28--..          -4

                     ^
    -4  -3  -2  -1  X=0  1   2   3   4

The bottom right quarter is rotated by 90 degrees as per the "block 1"
growth from a power-of-2 corner.  This means it's not the same as the bottom
right of parts=4.  But the two upper parts are the same as in parts=4 and
parts=2.

As noted by David Applegate and Omar Pol in OEIS A153006, the three parts
replication means that N at the last level of a power-of-2 block is a
triangular number,

    depth=2^k-1
    N(depth) = (2^k-1)*2^k/2
             = triangular number depth*(depth+1)/2
    at X=(depth-1)/2, Y=-(depth+1)/2

For example depth=2^3-1=7 begins at N=7*8/2=28 and is at the lower right
corner X=(7-1)/2=3, Y=-(7+1)/2=-4.  If the depth is not such a 2^k-1 then
N(depth) is less than the triangular depth*(depth+1)/2.

=head1 FUNCTIONS

See L<Math::PlanePath/FUNCTIONS> for behaviour common to all path classes.

=over 4

=item C<$path = Math::PlanePath::ToothpickTree-E<gt>new ()>

=item C<$path = Math::PlanePath::ToothpickTree-E<gt>new (parts =E<gt> $integer)>

Create and return a new path object.  C<parts> can be 1, 2, 3 or 4.

=back

=head2 Tree Methods

=over

=item C<@n_children = $path-E<gt>tree_n_children($n)>

Return the children of C<$n>, or an empty list if C<$n> has no children
(including when C<$n E<lt> 0>, ie. before the start of the path).

The children are the new toothpicks added at the ends of C<$n> at the next
level.  This can be 0, 1 or 2 points.  For example N=24 has no children, N=8
has a single child N=12, and N=2 has two children N=4,N=5.  The way points
are numbered means that two children are consecutive N values.

=item C<$n_parent = $path-E<gt>tree_n_parent($n)>

Return the parent node of C<$n>, or C<undef> if no parent due to C<$n E<lt>=
0> (the start of the path).

=item C<$depth = $path-E<gt>tree_n_to_depth($n)>

=item C<$n = $path-E<gt>tree_depth_to_n($depth)>

Return the depth of point C<$n>, or first C<$n> at given C<$depth>,
respectively.

The first point N=0 is depth=0 in all the "parts" forms.  The way parts=1
and parts=2 don't start at the origin means their depth at a given X,Y
differs by 2 or 1 respectively from the full pattern at the same point.

=back

=head1 FORMULAS

=head2 Depth to N

The first N at given depth is given by the formulas in the paper by
Applegate, Pol and Sloane above.  The first N is the total count of
toothpicks in the preceding levels.

It's convenient to calculate in terms of Nquad points within a single
quadrant, but with the depth always numbered in the style of parts=4 (not
the 1 or 2 offset of parts=2 or parts=1).

    depth = pow + rem
        where pow=2^k and 0 <= rem < 2^k

    Mquad(depth) = (4^k-4)/6          # 4^k = pow*pow
                /  0   if rem=0       # for depth=pow
             + |   1   if rem=1       # the "A" toothpick
               \   Ndepth(rem+1) + 2*Ndepth(rem) + 2  if rem>=2

    parts=1   Ndepth = Nquad(depth+2)
    parts=2   Ndepth = 2*Nquad(depth+1) + 1
    parts=3   Ndepth = 3*Nquad(depth+1) + 2
    parts=4   Ndepth = 4*Nquad(depth) + 3

For example depth=8 gives Nquad=(4^3-4)/6=10 which is the N at X=4,Y=4 of
parts=1.

Ndepth is then the Nquad with depth adjusted for whether the respective
parts=1,2,3,4 begin with depth=0 at the origin or 1 or 2, plus the initial
1, 2 or 3 points not in the Nquad replications.

The (4^k-4)/6 part is the total points in a 2*depth x 2*depth block, similar
to L</Level Ranges> above but a single quadrant.  It's a value "222..22" in
base-4, with k-1 many "2"s.  For example depth=8=2^3 has k=3 so k-1=2 many
"2"s for value "22" in base-4, which is 10.

The breakdown of depth to Ndepth(rem+1) + 2*Ndepth(rem) is the important
part of the formula.  It knocks out the high bit of depth and spreads the
remainder to an adjacent pair rem+1,rem.  This can be handled by keeping a
list of pending depth values desired and knocking them down with a pow=2^k,
then repeat with pow=2^(k-1), etc.

rem+1,rem are adjacent so successive reductions make a list growing by one further value each time, like

    d+1,d
    d+2,d+1,d
    d+3,d+2,d+1,d

But when the list crosses a 2^k boundary then some are reduced and others
remain.  When that happens the list is no longer successive values, only
mostly so.  When accumulating rem+1 and rem it's enough to check whether the
current rem+1 is equal to the "rem" of the previous breakdown and if so
coalesce with that previously entry.

The factor of 2 in 2*Ndepth(rem) can be handled by keeping a desired
multiplier with each pending depth.  Ndepth(rem+1) keeps the current
multiplier, and 2*Ndepth(rem) doubles the current.  When coalescing with a
previous entry then add to its multiplier.  Those additions mean the
multipliers are not powers-of-2.

If the pending list is a list of successive integers then rem+1,rem
breakdown and coalescing increases that list by just one value, keeping the
list to log2(depth) many entries (the number of bits in depth).  But as
noted above that's not so when the list crosses a 2^k boundary.  It then
behaves like two lists and each grow by one entry.  In any case the list
doesn't become huge.

=head2 N to Depth

The current C<tree_n_to_depth()> does a binary search for depth by calling
C<tree_depth_to_n()> on a successively narrower range.  Is there a better
approach?

Some intermediate values in the depth-to-N might be re-used by such repeated
calls, but it's not clear how many would be re-used and how many would be
needed only once.  The current code doesn't retain any such intermediates,
so large N can be handled without using a lot of memory.

=head1 OEIS

This cellular automaton is in Sloane's Online Encyclopedia of Integer
Sequences as

    http://oeis.org/A139250    (etc)

    parts=4
      A139250   total cells at given depth
      A139251    added cells at given depth
      A139253   total cells which are primes

      A147614   grid points covered at given depth
                 (including toothpick endpoints)

      A139252   line segments at given depth,
                 coalescing touching ends horiz or vert
      A139560   added segments, net of any new joins

      A162795   total cells parallel to initial (at X==Y mod 2)
      A162793    added parallel to initial
      A162796   total cells opposite to initial (at X!=Y mod 2)
      A162794    added opposite to initial
      A162797   difference total cells parallel - opposite

    parts=3
      A153006   total cells at given depth
      A152980    added cells at given depth
      A153009   total cells values which are primes

      A153007   difference depth*(depth+1)/2 - total cells,
                 which is 0 at depth=2^k-1

    parts=2
      A152998   total cells at given depth
      A152968    added cells at given depth
      A152999   total cells values which are primes

    parts=1
      A153000   total cells at given depth
      A152978    added cells at given depth
      A153002   total cells values which are primes

Drawings by Omar Pol

    parts=4
    http://www.polprimos.com/imagenespub/poltp4d4.jpg
    http://www.polprimos.com/imagenespub/poltp283.jpg

    parts=3
    http://www.polprimos.com/imagenespub/poltp028.jpg

    parts=1
    http://www.polprimos.com/imagenespub/poltp016.jpg


A153003, A153004, A153005 are another toothpick form where the parts=4 full
pattern is clipped to 3 quadrants.  This is not the same as the parts=3
corner pattern here.  The clipped form would have its X=1,Y=-1 cell either
as a "root" but at depth=1, or as a 3rd child of X=0,Y=1.  Allowing the
X=0,Y=0 and X=0,Y=-1 cells to be included would be a joined-up pattern, but
then the depth totals would be 2 bigger than those OEIS entries.

=head1 SEE ALSO

L<Math::PlanePath>,
L<Math::PlanePath::ToothpickReplicate>,
L<Math::PlanePath::LCornerTree>,
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
