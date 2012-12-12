# Copyright 2012 Kevin Ryde

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

package Math::PlanePath::SurroundOneEightByCells;             
use 5.004;
use strict;
use Carp;
#use List::Util 'max';
*max = \&Math::PlanePath::_max;

use vars '$VERSION', '@ISA';
$VERSION = 1;
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
  [ { name      => 'start',
      share_key => 'start_upstarplus',
      display   => 'Start',
      type      => 'enum',
      default   => 'right',
      choices   => ['one','two','three','four'],
    },
  ];


my @dir_to_dx = (1,1,0,-1, -1,-1,0,1);
my @dir_to_dy = (0,1,1,1,  0,-1,-1,-1);

sub new {
  my $self = shift->SUPER::new(@_);
  $self->{'sq'} = Math::PlanePath::SquareSpiral->new (n_start => 0);

  my $start = ($self->{'start'} ||= 'one');
  my @n_to_x;
  my @n_to_y;
  if ($start eq 'one') {
    @n_to_x = (0);
    @n_to_y = (0);
  } elsif ($start eq 'two') {
    @n_to_x = (0, -1);
    @n_to_y = (0, 0);
  } elsif ($start eq 'three') {
    @n_to_x = (0, -1, -1);
    @n_to_y = (0, 0, -1);
  } elsif ($start eq 'four') {
    @n_to_x = (0, -1, -1, 0);
    @n_to_y = (0, 0, -1, -1);
  } else {
    croak "Unrecognised start: ",$start;
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
  $self->{'xy_to_n'} = \@xy_to_n;

  ### xy_to_n: $self->{'xy_to_n'}
  ### endpoints: $self->{'endpoints'}

  return $self;
}

my @surround_dx = (1, 1, 0, -1, -1, -1,  0,  1);
my @surround_dy = (0, 1, 1,  1,  0, -1, -1, -1);

sub _extend {
  my ($self) = @_;
  ### _extend() ...

  my $sq = $self->{'sq'};
  my $endpoints = $self->{'endpoints'};
  my $xy_to_n = $self->{'xy_to_n'};
  my $n_to_x = $self->{'n_to_x'};
  my $n_to_y = $self->{'n_to_y'};

  ### depth: scalar(@{$self->{'depth_to_n'}})
  ### endpoints count: scalar(@$endpoints)

  my @new_endpoints;
  my @new_x;
  my @new_y;

  foreach my $endpoint_sn (@$endpoints) {
    my ($x,$y) = $sq->n_to_xy($endpoint_sn);
    ### endpoint: "$x,$y"

  SURROUND: foreach my $i (0 .. $#surround_dx) {
      my $x = $x + $surround_dx[$i];
      my $y = $y + $surround_dy[$i];
      ### consider: "$x,$y"
      my $sn = $sq->xy_to_n($x,$y);

      my $count = 0;
      foreach my $j (0 .. $#surround_dx) {
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

my $stop = 725000;
sub n_to_xy {
  my ($self, $n) = @_;
  ### SurroundOneEightByCells n_to_xy(): $n

  if ($n < 0) { return; }
  if (is_infinite($n)) { return ($n,$n); }

  if ($n > $stop) {
    return;
  }
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
  ### SurroundOneEightByCells xy_to_n(): "$x, $y"

  my $depth = 2 * (abs($x)+abs($y));
  if (is_infinite($depth)) {
    return (1,$depth);
  }

  ### $depth
  while (scalar(@{$self->{'depth_to_n'}}) <= $depth) {
    _extend($self);
  }

  my $sn = $self->{'sq'}->xy_to_n($x,$y);
  return $self->{'xy_to_n'}->[$sn];
}

# not exact
sub rect_to_n_range {
  my ($self, $x1,$y1, $x2,$y2) = @_;
  ### SurroundOneEightByCells rect_to_n_range(): "$x1,$y1  $x2,$y2"

  $x1 = round_nearest ($x1);
  $y1 = round_nearest ($y1);
  $x2 = round_nearest ($x2);
  $y2 = round_nearest ($y2);

  my $depth = 4 * max(1,
                      abs($x1),
                      abs($x2),
                      abs($y1),
                      abs($y2));
  return (0, $depth*$depth);
}

sub tree_depth_to_n {
  my ($self, $depth) = @_;
  my $depth_to_n = $self->{'depth_to_n'};
  while ($#$depth_to_n <= $depth) {
    _extend($self);
  }
  return $depth_to_n->[$depth];
}
sub n_to_depth {
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
    or return undef;
  ### $x
  ### $y

  my $depth = $self->n_to_depth($n) + 1;
  return grep { $self->n_to_depth($_) == $depth }
    map { $self->xy_to_n_list($x + $surround_dx[$_],
                              $y + $surround_dy[$_]) }
      0 .. $#surround_dx;
}
sub tree_n_parent {
  my ($self, $n) = @_;

  if ($n < 0) {
    return undef;
  }
  my ($x,$y) = $self->n_to_xy($n)
    or return undef;
  my $parent_depth = $self->n_to_depth($n) - 1;

  foreach my $i (0 .. $#surround_dx) {
    my $pn = $self->xy_to_n($x + $surround_dx[$i],
                            $y + $surround_dy[$i]);
    if (defined $pn && $self->n_to_depth($pn) == $parent_depth) {
      return $pn;
    }
  }
  return undef;
}

1;
__END__

=for stopwords eg Ryde Math-PlanePath-Toothpick Ulam Warburton Nstart Nend

=head1 NAME

Math::PlanePath::SurroundOneEightByCells -- toothpick sequence

=head1 SYNOPSIS

 use Math::PlanePath::SurroundOneEightByCells;
 my $path = Math::PlanePath::SurroundOneEightByCells->new;
 my ($x, $y) = $path->n_to_xy (123);

=head1 DESCRIPTION

I<In progress ...>

This is a 1 of 8 growth cellular automaton following

=cut

# math-image --path=SurroundOneEightByCells --output=numbers --all --size=65x11

=pod

           5

           4

           3

           2

           1

      <- Y=0

          -1

          -2

          -3

          -4

          -5
                       ^
      -4   -3 -2  -1  X=0  1   2   3   4

=cut

# Each X,Y point is the centre of a three-pronged toothpick.  The toothpick is
# vertical on "even" points X+Y==0 mod 2, or horizontal on "odd" points X+Y==1
# mod 2.
#
# Points are numbered by each growth depth at the endpoints, and
# anti-clockwise around when there's a new point at both ends of an existing
# toothpick.

=pod

                                
                                
                                
               \   / \   /      
                \ /   \ /       
                 4     3----    
  \   /           \   /    /    
   \ /             \ /    /     
    1----           1----2----  
                          \     
                           \    
                                
                                
                                
        \   /       \   /          
         \ /         \ /           
     -----8           7----        
     \     \   / \   /             
      \     \ /   \ /              
  -----9-----4     3----           
      /       \   /    /    /      
     /         \ /    /    /       
                1----2----6----    
                      \    \       
                       \    \      
                        5----      
                       / \         
                      /   \        


=cut

# The start is N=1 and points N=2 and N=3 are added to the two ends of that
# toothpick.  Then points N=4,5,6,7 are added at those four ends.
#
# For points N=4,5,6,7 a new toothpick is only added at each far ends, not the
# "inner" positions X=1,Y=0 and X=-1,Y=0.  This is because those points are
# the ends of two toothpicks and would overlap.  X=1,Y=0 is the end of
# toothpicks N=4 and N=7, and X=-1,Y=0 the ends of N=5,N=6.  The rule is that
# when two ends meet like that nothing is added at that point.  The end of a
# toothpick is allowed to touch an existing toothpick.  The first time this
# happens is N=16.  Its left end touches N=4.
#
# The stair-step X=Y,X=Y-1 diagonal N=2,4,8,12,17,25,36,44,49 etc and similar
# in the other quadrants extend indefinitely.  The quarters to either side of
# the diagonals are filled in a self-similar fashion.

=head1 FUNCTIONS

See L<Math::PlanePath/FUNCTIONS> for behaviour common to all path classes.

=over 4

=item C<$path = Math::PlanePath::SurroundOneEightByCells-E<gt>new ()>

Create and return a new path object.

=back

=cut

# =head2 Tree Methods
#
# =over
#
# =item C<@n_children = $path-E<gt>tree_n_children($n)>
#
# Return the children of C<$n>, or an empty list if C<$n> has no children
# (including when C<$n E<lt> 1>, ie. before the start of the path).
#
# The children are the new toothpicks added at the ends of C<$n> in the next
# depth.  This can be none, one or two points.
#
# =cut
#
# #   For example N=8 has a single
# # child 12, N=24 has no children, or N=2 has two children 4,5.  The way points
# # are numbered means when there's two children they're consecutive N values.
#
# =item C<$num = $path-E<gt>tree_n_num_children($n)>
# 
# Return the number of children of C<$n>, or return C<undef> if C<$nE<lt>1>
# (ie. before the start of the path).
#
# =item C<$n_parent = $path-E<gt>tree_n_parent($n)>
#
# Return the parent node of C<$n>, or C<undef> if C<$n E<lt>= 1> (the start of
# the path).
#
# =back

=head1 SEE ALSO

L<Math::PlanePath>,
L<Math::PlanePath::UlamWarburton>

=head1 HOME PAGE

http://user42.tuxfamily.org/math-planepath/index.html

=head1 LICENSE

Copyright 2012 Kevin Ryde

This file is part of Math-PlanePath-Toothpick.

Math-PlanePath-Toothpick is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

Math-PlanePath-Toothpick is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along with
Math-PlanePath-Toothpick.  If not, see <http://www.gnu.org/licenses/>.

=cut
