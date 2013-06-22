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



# A168131 added squares+rectangles parts=3
# A160124 total squares+rectangles
# 	= 1+2*A139250(n)-A147614 the grid points covered
# A160125 added squares+rectangles
# A211008 squares of size 2^(k-1)
# A160126 total squares+rectangles / 2
# A160127 added squares+rectangles / 2
# A159786 area squares&rectangles
# A159787 3/4 area squares&rectangles
# A159788 1/2 area squares&rectangles
# A159789 1/4 area squares&rectangles


use 5.004;
use strict;
use Math::Prime::XS 0.23 'is_prime'; # version 0.23 fix for 1928099

use Test;
plan tests => 146;

use lib 't','xt';
use MyTestHelpers;
MyTestHelpers::nowarnings();
use MyOEIS;

use Math::PlanePath::Base::Digits
  'round_down_pow';
use Math::PlanePath::ToothpickTree;

# uncomment this to run the ### lines
# use Smart::Comments '###';


#------------------------------------------------------------------------------
# A160159 - parts=two_horiz added
# cf A160158 total is Depth_start

MyOEIS::compare_values
  (anum => 'A160159',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 'two_horiz');
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_width( $depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A160407 - parts=wedge added

MyOEIS::compare_values
  (anum => 'A160407',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 'wedge');
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_width( $depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A168002 - parts=1 total cells mod 2, starting depth=1
MyOEIS::compare_values
  (anum => 'A168002',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 1);
     my @got;
     for (my $depth = 1; @got < $count; $depth++) {
       my $n = $path->tree_depth_to_n($depth);
       push @got, $n % 2;
     }
     return \@got;
   });

# T(n) = A139250(n)   OFFSET=0
# Q(n) = (T(n)-3)/4, n >= 2   paper section 3 end page 12
#      = A153000(n-2)
# T(n) = 4*Q(n)+3
#
# Q(2^k + rem) = Q(rem+1) + 2*Q(rem) + 2
# Q(2^k + rem) == Q(rem+1) mod 2
# Q(2^k-1 + rem) == Q(rem) mod 2
# for rem>=1

# T(pow+rem) = T(pow) + T(rem+1) + 2*T(rem) - 1
# Q(pow+rem) = (T(pow+rem)-3)/4
#   = (T(pow)-3 + 3 + T(rem+1)-3 + 3 + 2*T(rem)-6 + 6 - 1 - 3) / 4
#   = (T(pow)-3)/4 + 3/4 + (T(rem+1)-3)/4 + 3/4 + (2*T(rem)-6)/4 + (6 - 1 - 3) / 4
#   = Q(pow) + Q(rem+1) + 2*Q(rem) + 3/4 + 3/4 + (6 - 1 - 3) / 4
#   = Q(pow) + Q(rem+1) + 2*Q(rem) + 2
#
# T(pow+rem) = T(pow) + T(rem+1) + 2*T(rem) - 1
# 4*Q(pow+rem)+3 = 4*Q(pow)+3 + 4*Q(rem+1)+3 + 2*(4*Q(rem)+3) - 1
# 4*Q(pow+rem) = 4*Q(pow) + 4*Q(rem+1) + 2*4*Q(rem) - 1 - 3+3+2*3+3
#              = 4*Q(pow) + 4*Q(rem+1) + 2*4*Q(rem) + 8
# Q(pow+rem) = Q(pow) + Q(rem+1) + 2*Q(rem) + 2
#
# Q(2^k) = (T(2^k)-3)/4
#        = ((2*4^k+1)/3 - 3) / 4
#        = ((2*4^k+1) - 9) / 12
#        = (2*4^k - 8) / 12
#        = (4^k - 4) / 6
# quad(pow) = (4^k-4)/6          # with 4^k = pow*pow
# quad(pow+1) = quad(pow) + 1   # the "A" toothpick
# and for rem>=2
# quad(pow+rem) = quad(pow) + quad(rem+1) + 2*quad(rem) + 2
# eg. quad(4+2) = quad(4) + quad(3) + 2*quad(2) + 2
#               =   2     +   1     + 2*0       + 2  = 5
# eg. quad(4+3) = quad(4) + quad(4) + 2*quad(3) + 2
#               = 2 + 2 + 2*1 + 2 = 8
#
sub my_A153000 {
  my ($n) = @_;
  ### my_A153000(): $n
  die "A153000($n)" if $n < 2;
  if ($n == 2) { return 0; }
  if ($n == 3) { return 1; }
  my ($pow,$exp) = round_down_pow($n,2);
  my $rem = $n - $pow;
  if ($rem == 0) { return ($pow*$pow - 4) / 6; }
  if ($rem == 1) { return my_A153000($pow) + 1; }
  return my_A153000($pow) + my_A153000($rem+1) + 2*my_A153000($rem) + 2;

  # my $q1 = my_A153000($rem+1);
  # my $q2 = 2*my_A153000($rem);
  # my $ret = $q1 + 2*$q2 + 2;
  # ### $pow
  # ### $rem
  # ### $q1
  # ### $q2
  # ### $ret;
  # return $ret;
  return 99;
}
BEGIN {
  use Memoize;
  Memoize::memoize('my_A153000');
}
MyOEIS::compare_values
  (anum => 'A153000',
   func => sub {
     my ($count) = @_;
     my @got;
     for (my $n = 2; @got < $count; $n++) {
       push @got, my_A153000($n);
     }
     return \@got;
   });

# cf A079559
# fixed point of 0->0 1->110
# 1                 1
# 110               3
# 1101100           7
# 110110011011000   15
# 01234567
# A(2^k-1 + rem) = A(rem)
# A(2^k-1 + 2^k-1) = 0
#
# 1 = 1
# 2 = no
# 3 = 3
# 4 = 3 + 1
# 5 = no
# 6 = no
# 7 = 7
# 8 = 7 + 1
# 9 = no
# 10 = 7 + 3
# 11 = 7 + 3 + 1
# 12 = no
# 13 = no
# 14 = no
# 1, 1, 0, 1, 1, 0, 0, 1, 1
#
# zeros run length A007814 count low 0-bits
# run lengths abs(A088705)+1  A088705=delta A000120 count 1-bits
# A005187 positions of 1s
# A055938 positions of 0s
MyOEIS::compare_values
  (anum => 'A079559',
   func => sub {
     my ($count) = @_;
     my @got;
     for (my $n = 0; @got < $count; $n++) {
       push @got, my_A079559($n);
     }
     return \@got;
   });
sub my_A079559 {
  my ($n) = @_;
  for (;;) {
    my ($pow,$exp) = round_down_pow($n+1,2);
    $n -= $pow-1;
    if ($n == 0) { return 1; }
    if ($n == $pow-1) { return 0; }
  }
}


#------------------------------------------------------------------------------
# A153002 parts=1 primes among total cells
MyOEIS::compare_values
  (anum => 'A153002',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 1);
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
# A152999 - parts=2 primes among parts=2 total cells
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
# A153001 parts=3 added cells infinite row not across 2^k boundary

MyOEIS::compare_values
  (anum => 'A153001',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 3);
     my ($depth,$exp) = round_down_pow($count,2);
     $depth *= 4;
     my @got;
     for ( ; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_width( $depth);
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

# A159791 total cells depth=even
MyOEIS::compare_values
  (anum => 'A159791',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new;
     my @got;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth+=2) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

# A159792 total cells depth=odd
MyOEIS::compare_values
  (anum => 'A159792',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new;
     my @got;
     my $total = 0;
     for (my $depth = 1; @got < $count; $depth+=2) {
       push @got, $path->tree_depth_to_n($depth);
     }
     return \@got;
   });

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
# A153009 primes among parts=3 total cells

MyOEIS::compare_values
  (anum => 'A153009',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 3);
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
# A153007 triangular n(n+1)/2 subtract toothpick total parts=3

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
# triangular = 2^k*(2^k-1)/2 = 1,6,28,120,496,...
{
  my $path = Math::PlanePath::ToothpickTree->new (parts => 3);
  require Math::BigInt;
  for (my $i = 0; $i < 128; $i++) {
    my $depth = Math::BigInt->new(1)->blsft($i)->bsub(1);  # 2^i-1
    my $n = $path->tree_depth_to_n($depth);
    my $diff = $depth*($depth+1)/2 - $n;
    ok ($diff, 0);
    my ($x,$y) = $path->n_to_xy($n);
    ok ($x, ($depth-1)/2,  "X at depth=$depth");
    ok ($y, -($depth+1)/2, "Y at depth=$depth");
  }
}

#------------------------------------------------------------------------------
# A152978 - parts=1 added

MyOEIS::compare_values
  (anum => 'A152978',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 1);
     my @got;
     for (my $depth = 0; @got < $count; $depth++) {
       push @got, $path->tree_depth_to_width( $depth);
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
       push @got, $path->tree_depth_to_width( $depth);
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
       push @got, $path->tree_depth_to_width( $depth);
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
       push @got, $path->tree_depth_to_width( $depth);
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
       $total -= $path->tree_depth_to_width( $depth);
       $total += $path->tree_depth_to_width( $depth+1);
       push @got, $total;
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A162794 added opposite to initial, being horizontal toothpicks
# A162796 total opposite to initial

MyOEIS::compare_values
  (anum => 'A162794',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
     my @got = (0);
     my $n = $path->n_start;
     for (my $depth = 1; @got < $count; $depth+=2) {
       push @got, $path->tree_depth_to_width( $depth);
     }
     return \@got;
   });

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
       $total += $path->tree_depth_to_width( $depth);
     }
     return \@got;
   });

#------------------------------------------------------------------------------
# A162795 total parallel to initial, being vertical toothpicks
# A162793 added parallel to initial, being vertical toothpicks

MyOEIS::compare_values
  (anum => 'A162793',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
     my @got;
     my $n = $path->n_start;
     for (my $depth = 0; @got < $count; $depth+=2) {
       push @got, $path->tree_depth_to_width( $depth);
     }
     return \@got;
   });

MyOEIS::compare_values
  (anum => 'A162795',
   func => sub {
     my ($count) = @_;
     my $path = Math::PlanePath::ToothpickTree->new (parts => 4);
     my @got;
     my $n = $path->n_start;
     my $total = 0;
     for (my $depth = 0; @got < $count; $depth += 2) {
       $total += $path->tree_depth_to_width( $depth);
       push @got, $total;
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
exit 0;
