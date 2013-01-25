#!/usr/bin/perl -w

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


use 5.004;
use strict;
use Test;
plan tests => 4;

use lib 't','xt','devel/lib';
use MyTestHelpers;
MyTestHelpers::nowarnings();
use MyOEIS;

use Math::PlanePath::ToothpickByCells;

# uncomment this to run the ### lines
#use Smart::Comments '###';

my $max_count = undef;

#------------------------------------------------------------------------------
# A170890 - downwedge2 total cells

MyOEIS::compare_values
  (anum => 'A170888',
   max_count => $max_count,
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickByCells->new (parts => 'downwedge2');
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A170891 - downwedge2 added

MyOEIS::compare_values
  (anum => 'A170889',
   max_count => $max_count,
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickByCells->new (parts => 'downwedge2');
     my @got = (0);
     for (my $depth = 0; @got < $count; $depth++) {
       my $added = ($path->tree_depth_to_n($depth+1)
                    - $path->tree_depth_to_n($depth));
       push @got, $added;
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A170888 - outwedge total cells

MyOEIS::compare_values
  (anum => 'A170888',
   max_count => $max_count,
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickByCells->new (parts => 'outwedge');
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A170889 - outwedge added

MyOEIS::compare_values
  (anum => 'A170889',
   max_count => $max_count,
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickByCells->new (parts => 'outwedge');
     my @got = (0);
     for (my $depth = 0; @got < $count; $depth++) {
       my $added = ($path->tree_depth_to_n($depth+1)
                    - $path->tree_depth_to_n($depth));
       push @got, $added;
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A170886 - outwedge2 total cells

MyOEIS::compare_values
  (anum => 'A170886',
   max_count => $max_count,
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickByCells->new (parts => 'outwedge2');
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A170887 - outwedge2 added

MyOEIS::compare_values
  (anum => 'A170887',
   max_count => $max_count,
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickByCells->new (parts => 'outwedge2');
     my @got = (0);
     for (my $depth = 0; @got < $count; $depth++) {
       my $added = ($path->tree_depth_to_n($depth+1)
                    - $path->tree_depth_to_n($depth));
       push @got, $added;
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A160406 - wedge total cells

MyOEIS::compare_values
  (anum => 'A160406',
   max_count => $max_count,
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickByCells->new (parts => 'wedge');
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A160407 - wedge added

MyOEIS::compare_values
  (anum => 'A160407',
   max_count => $max_count,
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickByCells->new (parts => 'wedge');
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       my $added = ($path->tree_depth_to_n($depth+1)
                    - $path->tree_depth_to_n($depth));
       push @got, $added;
     }
     return \@got;
   });

#------------------------------------------------------------------------------
exit 0;
