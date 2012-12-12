#!/usr/bin/perl -w

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


use 5.004;
use strict;
use Math::Prime::XS 0.23 'is_prime'; # version 0.23 fix for 1928099

use Test;
plan tests => 146;

use lib 't','xt';
use MyTestHelpers;
MyTestHelpers::nowarnings();
use MyOEIS;

use Math::PlanePath::ToothpickTree;

# uncomment this to run the ### lines
#use Smart::Comments '###';


#------------------------------------------------------------------------------
# A152978 - parts=1 added

MyOEIS::compare_values
  (anum => 'A152978',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 1);
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, ($path->tree_depth_to_n($depth+1)
                   - $path->tree_depth_to_n($depth));
     }
     return \@got;
   });


#------------------------------------------------------------------------------
# A152968 - parts=2 added

MyOEIS::compare_values
  (anum => 'A152968',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 2);
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, ($path->tree_depth_to_n($depth+1)
                   - $path->tree_depth_to_n($depth));
     }
     return \@got;
   });


#------------------------------------------------------------------------------
# A152980 - parts=3 added

MyOEIS::compare_values
  (anum => 'A152980',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 3);
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, ($path->tree_depth_to_n($depth+1)
                   - $path->tree_depth_to_n($depth));
     }
     return \@got;
   });


#------------------------------------------------------------------------------
# A139251 - parts=4 added

MyOEIS::compare_values
  (anum => 'A139251',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
     my @got = (0);
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, ($path->tree_depth_to_n($depth+1)
                   - $path->tree_depth_to_n($depth));
     }
     return \@got;
   });


#------------------------------------------------------------------------------
# A139252 total line segments, touching endpoints coalesced

{
  my @got;

  MyOEIS::compare_values
      (anum => 'A139252',
       func => sub {
         my ($count) = @_;
         my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
         my %hseen;
         my %vseen;
         my $n = $path->n_start;
         my $segs = 0;
         for (my $depth = -1; @got < $count; $depth++) {
           my $next_n = $path->tree_depth_to_n($depth+1);
           for ( ; $n < $next_n; $n++) {
             my ($x,$y) = $path->n_to_xy($n);

             # +1 if isolated, 0 if extends one end of segment, -1 if joins two
             # segments
             if ($depth & 1) {
               $segs++;
               my $n2;
               $segs -= (defined($n2 = $path->xy_to_n($x-2,$y))
                         && $n2 < $n);
               $segs -= (defined($n2 = $path->xy_to_n($x+2,$y))
                         && $n2 < $n);
             } else {
               $segs++;
               my $n2;
               $segs -= (defined($n2 = $path->xy_to_n($x,$y-2))
                         && $n2 < $n);
               $segs -= (defined($n2 = $path->xy_to_n($x,$y+2))
                         && $n2 < $n);
             }
           }
           push @got, $segs;
         }
         return \@got;
       });

  MyOEIS::compare_values
      (anum => 'A139560',
       func => sub {
         my ($count) = @_;
         foreach my $i (1 .. $#got) {
           $got[$i-1] = $got[$i] - $got[$i-1];
         }
         while (@got > $count) {
           pop @got;
         }
         return \@got;
       });
}

# sub path_xy_to_depth {
#   my ($path, $x,$y) = @_;
#   my $n = $path->xy_to_n($x,$y);
#   return $n && $path->tree_n_to_depth($n);
# }


#------------------------------------------------------------------------------
# A153007 triangular n(n+1)/2 - toothpick total parts=3

MyOEIS::compare_values
  (anum => 'A153007',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 3);
     my $n = $path->n_start;
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $depth*($depth+1)/2 - $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

# at depth=2^k-1 have diff==0
# triangular = 2^k*(2^k-1)/2
{
  my $path = Math::PlanePath::ToothpickTree->new (parts => 3);
  require Math::BigInt;
  for (my $i = 0; $i < 128; $i++) {
    my $depth = Math::BigInt->new(1)->blsft($i)->bsub(1);
    my $diff = $depth*($depth+1)/2 - $path->tree_depth_to_n($depth);
    ok ($diff, 0);
  }
}

#------------------------------------------------------------------------------
# A162797 difference parallel-opposite

MyOEIS::compare_values
  (anum => 'A162797',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
     my $n = $path->n_start;
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth+=2) {
       $total -= ($path->tree_depth_to_n($depth+1)
                  - $path->tree_depth_to_n($depth));
       $total += ($path->tree_depth_to_n($depth+2)
                  - $path->tree_depth_to_n($depth+1));
       push @got, $total;
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A162794 added opposite to initial

MyOEIS::compare_values
  (anum => 'A162794',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
     my @got = (0);
     my $n = $path->n_start;
     for (my $depth = 1; @got < $count; $depth+=2) {
       push @got, ($path->tree_depth_to_n($depth+1)
                   - $path->tree_depth_to_n($depth));
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A162793 added parallel to initial

MyOEIS::compare_values
  (anum => 'A162793',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
     my @got;
     my $n = $path->n_start;
     for (my $depth = 0; @got < $count; $depth+=2) {
       push @got, ($path->tree_depth_to_n($depth+1)
                   - $path->tree_depth_to_n($depth));
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A162795 total parallel to initial

MyOEIS::compare_values
  (anum => 'A162795',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
     my @got;
     my $n = $path->n_start;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth+=2) {
       $total += ($path->tree_depth_to_n($depth+1)
                  - $path->tree_depth_to_n($depth));
       push @got, $total;
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A162796 total opposite to initial

MyOEIS::compare_values
  (anum => 'A162796',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
     my @got;
     my $n = $path->n_start;
     my $total = 0;
     for (my $depth = 1; @got < $count; $depth+=2) {
       push @got, $total;
       $total += ($path->tree_depth_to_n($depth+1)
                  - $path->tree_depth_to_n($depth));
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A147614 grid points if length 2

MyOEIS::compare_values
  (anum => 'A147614',
   # max_value=>10000,
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
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
# A139250 - parts=4 total cells

MyOEIS::compare_values
  (anum => 'A139250',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new;
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });


#------------------------------------------------------------------------------
# A139253 - parts=4 primes in A139250 total cells

MyOEIS::compare_values
  (anum => 'A139253',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new;
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       my $n = $path->tree_depth_to_n($depth);
       if (is_prime($n)) {
         push @got, $n;
       }
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A152998 - parts=2 total cells

MyOEIS::compare_values
  (anum => 'A152998',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 2);
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A152999 primes among parts=2 total cells

MyOEIS::compare_values
  (anum => 'A152999',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 2);
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       my $n = $path->tree_depth_to_n($depth);
       if (is_prime ($n)) {
         push @got, $n;
       }
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A153000 - parts=1 total cells

MyOEIS::compare_values
  (anum => 'A153000',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 1);
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
exit 0;
