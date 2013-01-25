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


# Cell ON at 1 of 2 vertically on odd cells X!=Y mod 2
#         at 1 of 2 horizontally on even cells X=Y mod 2
# is same as ToothpickTree.
#
# wedge
# A160406 total cells
# A160407 added
# poltp406.jpg
# A170886 Similar to A160406, always staying outside the wedge, but starting
# with a toothpick whose midpoint touches the vertex of the wedge.
#
# outwedge    diagonal stair step only
# A170888 total cells
# A170889 added
# A170888 Similar to A160406, always staying outside the wedge, but starting
# with a vertical half-toothpick which protrudes from the vertex of the
# wedge.
#
# outwedge2   extra at left end of wedge region
# A170886 total cells
# A170887 added
# A170886 Similar to A160406, always staying outside the wedge, but starting
# with a toothpick whose midpoint touches the vertex of the wedge.

# A170890 Similar to A160406, always staying outside the wedge, but starting with a horizontal half-toothpick which protrudes from the vertex of the wedge.
# A170891 First differences of A170890.

# A170892 Similar to A160406, always staying outside the wedge, but starting with a vertical toothpick whose endpoint touches the vertex of the wedge.
# A170893 First differences of A170892.

# A170894 Similar to A160406, always staying outside the wedge, but starting with a horizontal toothpick whose endpoint touches the vertex of the wedge.
# A170895 First differences of A170894.

package Math::PlanePath::ToothpickByCells;
use 5.004;
use strict;
use Carp;
#use List::Util 'max';
*max = \&Math::PlanePath::_max;

use vars '$VERSION', '@ISA';
$VERSION = 2;
use Math::PlanePath;
@ISA = ('Math::PlanePath');

use Math::PlanePath::Base::Generic
  'is_infinite',
  'round_nearest';
use Math::PlanePath::Base::Digits
  'round_down_pow';
use Math::PlanePath::SquareSpiral;

# uncomment this to run the ### lines
# use Smart::Comments;


use constant n_start => 0;

use constant parameter_info_array =>
  [ { name      => 'parts',
      share_key => 'parts_upstarplus',
      display   => 'Parts',
      type      => 'enum',
      default   => 'one',
      choices   => ['one','two','three','cross','octant',
                    'wedge','outwedge','outwedge2','outwedge3',
                    'downwedge','downwedge2'],
    },
  ];


sub new {
  my $self = shift->SUPER::new(@_);
  $self->{'sq'} = Math::PlanePath::SquareSpiral->new (n_start => 0);

  my $parts = ($self->{'parts'} ||= 'one');
  my @n_to_x;
  my @n_to_y;
  my @endpoint_dirs;
  if ($parts eq 'one' || $parts eq 'octant'
      || $parts eq 'wedge'
      || $parts eq 'outwedge' || $parts eq 'outwedge2' || $parts eq 'outwedge3'
      || $parts eq 'downwedge' || $parts eq 'downwedge2'
     ) {
    @n_to_x = (0);
    @n_to_y = (0);
    @endpoint_dirs = (2);
  } elsif ($parts eq 'two') {
    @n_to_x = (0, -2);
    @n_to_y = (0, 0);
    @endpoint_dirs = (2, 0);
  } elsif ($parts eq 'three') {
    @n_to_x = (0, -1, -1);
    @n_to_y = (0, 0, -1);
    @endpoint_dirs = (2, 3, 0);
  } elsif ($parts eq 'cross') {
    @n_to_x = (0, -1, 1, 0);
    @n_to_y = (0, 0, 0, -2);
    @endpoint_dirs = (2, 3, 0, 1);
  } else {
    croak "Unrecognised parts: ",$parts;
  }
  $self->{'n_to_x'} = \@n_to_x;
  $self->{'n_to_y'} = \@n_to_y;
  $self->{'depth_to_n'} = [0];

  my @endpoints;
  my @xy_to_n;
  foreach my $n (0 .. $#n_to_x) {
    my $sn = $self->{'sq'}->xy_to_n($n_to_x[$n],$n_to_y[$n]);
    $xy_to_n[$sn] = $n;
    push @endpoints, $sn;
  }
  $self->{'endpoints'} = \@endpoints;
  $self->{'endpoint_dirs'} = \@endpoint_dirs;
  $self->{'xy_to_n'} = \@xy_to_n;

  ### xy_to_n: $self->{'xy_to_n'}
  ### endpoints: $self->{'endpoints'}

  return $self;
}

my @surround_dx = (0, 0, 1, -1);
my @surround_dy = (1, -1, 0, 0);

sub _extend {
  my ($self) = @_;
  ### _extend() ...

  my $sq = $self->{'sq'};
  my $endpoints = $self->{'endpoints'};
  my $xy_to_n = $self->{'xy_to_n'};
  my $n_to_x = $self->{'n_to_x'};
  my $n_to_y = $self->{'n_to_y'};
  my $parts = $self->{'parts'};

  my $depth = scalar(@{$self->{'depth_to_n'}});
  ### $depth
  ### endpoints count: scalar(@$endpoints)

  my @new_endpoints;
  my @new_x;
  my @new_y;

  foreach my $endpoint_sn (@$endpoints) {
    my ($x,$y) = $sq->n_to_xy($endpoint_sn);
    ### endpoint: "$x,$y"

  SURROUND: foreach my $i ($depth & 1 ? (0..1) : (2..3)) {
      my $x = $x + $surround_dx[$i];
      my $y = $y + $surround_dy[$i];
      if ($parts eq 'octant') {
        if ($y <= 0 || $y > $x+1) { next; }
      }
      if ($parts eq 'wedge') {
        if (abs($x) > $y) { next; }
      }
      if ($parts eq 'outwedge') {
        if ($x < -abs($y)) { next; }
      }
      if ($parts eq 'outwedge2') {
        if ($x <= -abs($y)) { next; }
      }
      if ($parts eq 'outwedge3') {
        if ($x <= -abs($y)+1) { next; }
      }
      if ($parts eq 'downwedge') {
        if ($y <= -abs($x)) { next; }
      }
      if ($parts eq 'downwedge2') {
        if ($y < -abs($x)) { next; }
      }
      ### consider: "$x,$y"
      my $sn = $sq->xy_to_n($x,$y);
      if (defined($xy_to_n->[$sn])) { next; } # if already occupied

      my $count = 0;
      # foreach my $j (0 .. 3) {       # four around
      foreach my $j ($depth & 1 ? (0..1) : (2..3)) {  # VH around
        my $x = $x + $surround_dx[$j];
        my $y = $y + $surround_dy[$j];
        my $sn = $sq->xy_to_n($x,$y);
        ### count: "$x,$y at sn=$sn is ".($xy_to_n->[$sn] // 'undef')
        if (defined($xy_to_n->[$sn])) {
          if ($count++) {
            ### two or more surround ...
            next SURROUND;
          }
        }
      }
      push @new_endpoints, $sn;
      push @new_x, $x;
      push @new_y, $y;
    }
  }

  my $n = scalar(@$n_to_x);
  push @{$self->{'depth_to_n'}}, $n;
  foreach my $sn (@new_endpoints) {
    $xy_to_n->[$sn] = $n++;
  }
  push @$n_to_x, @new_x;
  push @$n_to_y, @new_y;

  $self->{'endpoints'} = \@new_endpoints;
}

sub n_to_xy {
  my ($self, $n) = @_;
  ### ToothpickByCells n_to_xy(): $n

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

  while ($#{$self->{'n_to_x'}} < $n) {
    _extend($self);
  }

  ### x: $self->{'n_to_x'}->[$n]
  ### y: $self->{'n_to_y'}->[$n]
  return ($self->{'n_to_x'}->[$n],
          $self->{'n_to_y'}->[$n]);
}

sub xy_to_n {
  my ($self, $x, $y) = @_;
  ### ToothpickByCells xy_to_n(): "$x, $y"

  my ($depth,$exp) = round_down_pow (max($x,$y), 2);
  $depth *= 4;
  if (is_infinite($depth)) {
    return (1,$depth);
  }

  ### $depth
  for (;;) {
    {
      my $sn = $self->{'sq'}->xy_to_n($x,$y);
      if (defined (my $n = $self->{'xy_to_n'}->[$sn])) {
        return $n;
      }
    }
    if (scalar(@{$self->{'depth_to_n'}}) <= $depth) {
      _extend($self);
    } else {
      return undef;
    }
  }
}

# not exact
sub rect_to_n_range {
  my ($self, $x1,$y1, $x2,$y2) = @_;
  ### ToothpickByCells rect_to_n_range(): "$x1,$y1  $x2,$y2"

  $x1 = round_nearest ($x1);
  $y1 = round_nearest ($y1);
  $x2 = round_nearest ($x2);
  $y2 = round_nearest ($y2);

  my $depth = 8 * max(1,
                      abs($x1),
                      abs($x2),
                      abs($y1),
                      abs($y2));
  return (0, $depth*$depth);
}

sub tree_depth_to_n {
  my ($self, $depth) = @_;
  if ($depth < 0) {
    return undef;
  }
  if (is_infinite($depth)) {
    return $depth;
  }
  my $depth_to_n = $self->{'depth_to_n'};
  while ($#$depth_to_n <= $depth) {
    _extend($self);
  }
  return $depth_to_n->[$depth];
}
sub tree_n_to_depth {
  my ($self, $n) = @_;

  if ($n < 0) {
    return undef;
  }
  if (is_infinite($n)) {
    return $n;
  }
  my $depth_to_n = $self->{'depth_to_n'};
  for (my $depth = 1; ; $depth++) {
    while ($depth > $#$depth_to_n) {
      _extend($self);
    }
    if ($n < $depth_to_n->[$depth]) {
      return $depth-1;
    }
  }
}

sub tree_n_children {
  my ($self, $n) = @_;
  ### tree_n_children(): $n

  my ($x,$y) = $self->n_to_xy($n)
    or return;
  ### $x
  ### $y

  my @n = map { $self->xy_to_n($x+$surround_dx[$_],$y+$surround_dy[$_]) }
    0 .. $#surround_dx;
  my $child_depth = $self->tree_n_to_depth($n) + 1;
  ### $child_depth

  ### @n
  # ### depths: map {defined $_ && $n_to_depth->[$_]} @n

  @n = sort {$a<=>$b}
    grep {defined $_ && $self->tree_n_to_depth($_) == $child_depth}
      @n;
  ### found: @n
  return @n;
}
sub tree_n_parent {
  my ($self, $n) = @_;

  my ($x,$y) = $self->n_to_xy($n)
    or return undef;
  my $parent_depth = $self->tree_n_to_depth($n) - 1;
  ### $parent_depth

  foreach my $dir (0 .. $#surround_dx) {
    if (defined (my $n = $self->xy_to_n($x+$surround_dx[$dir],
                                        $y+$surround_dy[$dir]))) {
      if ($self->tree_n_to_depth($n) == $parent_depth) {
        return $n;
      }
    }
  }
  return undef;
}

1;
__END__
