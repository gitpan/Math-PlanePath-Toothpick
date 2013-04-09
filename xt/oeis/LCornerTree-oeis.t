#!/usr/bin/perl -w

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


use 5.004;
use strict;
use Test;
plan tests => 4;

use lib 't','xt';
use MyTestHelpers;
MyTestHelpers::nowarnings();
use MyOEIS;

use Math::PlanePath::LCornerTree;

# uncomment this to run the ### lines
#use Smart::Comments '###';

# Return the number of points at $depth.
sub path_tree_depth_to_width {
  my ($path, $depth) = @_;
  if (defined (my $n = $path->tree_depth_to_n($depth))
      && defined (my $n_end = $path->tree_depth_to_n_end($depth))) {
    return $n_end - $n + 1;
  } else {
    return undef;
  }
}

#------------------------------------------------------------------------------
# A162784 - added cells parts=octant

MyOEIS::compare_values
  (anum => 'A162784',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::LCornerTree->new (parts => 'octant');
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, path_tree_depth_to_width($path,$depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A160410 - total cells parts=4

MyOEIS::compare_values
  (anum => 'A160410',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::LCornerTree->new;
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A161411 - added cells parts=4

MyOEIS::compare_values
  (anum => 'A161411',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::LCornerTree->new;
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, path_tree_depth_to_width($path,$depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A160412 - total cells parts=3

MyOEIS::compare_values
  (anum => 'A160412',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::LCornerTree->new (parts => 3);
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A162349 - added cells parts=3

MyOEIS::compare_values
  (anum => 'A162349',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::LCornerTree->new (parts => 3);
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, path_tree_depth_to_width($path,$depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A130665 - total cells parts=1

MyOEIS::compare_values
  (anum => 'A130665',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::LCornerTree->new (parts => 1);
     my @got;
     for (my $depth = 1; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A048883 - added cells parts=1

MyOEIS::compare_values
  (anum => 'A048883',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::LCornerTree->new (parts => 1);
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, path_tree_depth_to_width($path,$depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
exit 0;
