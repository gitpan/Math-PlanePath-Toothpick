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
plan tests => 7;

use lib 't','xt';
use MyTestHelpers;
MyTestHelpers::nowarnings();
use MyOEIS;

use Math::PlanePath::ToothpickUpist;


#------------------------------------------------------------------------------
# A175098 grid points covered by length=2

MyOEIS::compare_values
  (anum => 'A175098',
   # max_value=>10000,
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickUpist->new (parts => 4);
     my @got;
     my %seen;
     my $n = $path->n_start;
     for (my $depth = -1; @got < $count; $depth++) {
       my $next_n = $path->tree_depth_to_n($depth+1);
       for ( ; $n < $next_n; $n++) {
         my ($x,$y) = $path->n_to_xy($n);
         $seen{"$x,$y"} = 1;

         if ($depth & 1) {
           $seen{($x+1).",$y"} = 1;
           $seen{($x-1).",$y"} = 1;
         } else {
           $seen{"$x,".($y+1)} = 1;
           $seen{"$x,".($y-1)} = 1;
         }
       }
       push @got, scalar(keys %seen);
     }
     return \@got;
   });


#------------------------------------------------------------------------------
# A160745 - added*3

MyOEIS::compare_values
  (anum => 'A160745',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickUpist->new;
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       my $added = ($path->tree_depth_to_n($depth+1)
                    - $path->tree_depth_to_n($depth));
       push @got, 3 * $added;
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A160742 - total*2

MyOEIS::compare_values
  (anum => 'A160742',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickUpist->new;
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, 2 * $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A160744 - total*3

MyOEIS::compare_values
  (anum => 'A160744',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickUpist->new;
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, 3 * $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A160746 - total*4

MyOEIS::compare_values
  (anum => 'A160746',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickUpist->new;
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, 4 * $path->tree_depth_to_n($depth);
     }
     return \@got;
   });


#------------------------------------------------------------------------------
# A151566 - total cells leftist

MyOEIS::compare_values
  (anum => 'A151566',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickUpist->new;
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A060632,A151565 - cells added leftist

MyOEIS::compare_values
  (anum => 'A060632',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickUpist->new;
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       my $added = ($path->tree_depth_to_n($depth+1)
                    - $path->tree_depth_to_n($depth));
       push @got, $added;
     }
     return \@got;
   });

MyOEIS::compare_values
  (anum => 'A151565',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickUpist->new;
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
