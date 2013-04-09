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

use 5.010;
use strict;
use Math::PlanePath::LCornerReplicate;
use List::Util 'min', 'max';
use Math::PlanePath::Base::Digits
  'round_down_pow',
  'bit_split_lowtohigh',
  'digit_split_lowtohigh',
  'digit_join_lowtohigh';
use constant _PI => 4 * atan2(1,1);  # similar to Math::Complex

{
  # max Dir4

  require Math::BaseCnv;
  require Math::NumSeq::PlanePathDelta;

  eval 'use Math::BigInt try=>q{GMP}; 1'
    || eval 'use Math::BigInt; 1'
      || die;
  require Math::BigFloat;
  my $zero = Math::BigInt->new;
  my $power = Math::BigInt->new(1) << (4*50);

  my $radix = 16;
  my $planepath = "LCornerReplicate";
  my $seq = Math::NumSeq::PlanePathDelta->new (planepath => $planepath,
                                               delta_type => 'Dir4',
                                               # delta_type => 'TDir6',
                                               #delta_type => 'dX',
                                              );
  my $dx_seq = Math::NumSeq::PlanePathDelta->new (planepath => $planepath,
                                                  delta_type => 'dX');
  my $dy_seq = Math::NumSeq::PlanePathDelta->new (planepath => $planepath,
                                                  delta_type => 'dY');
  my $min = 99;
  my $max = -99;
  for my $len (1 .. 100) {
    # "3233...33"
    my @idigits = (((3)x$len), 2, 3);
    my $i = digit_join_lowtohigh(\@idigits,4,$zero);
    my $value = $seq->ith($i);

    if ($value > $max) {
      my $dx = $dx_seq->ith($i);
      my $dy = $dy_seq->ith($i);
      my $ri = Math::BaseCnv::cnv($i,10,$radix);
      my $rdx = Math::BaseCnv::cnv($dx,10,$radix);
      my $rdy = Math::BaseCnv::cnv($dy,10,$radix);
      my $f = $dy && $dx/$dy;
      printf "max i=%d[%s] %.5f  dx=%s,dy=%s[%s,%s]   %.3f\n",
        $i,$ri, $value, $dx,$dy, $rdx,$rdy, $f;

      my $ratio = Math::BigFloat->new($dy) / Math::BigFloat->new($dx);
      my $pratio = $dy * $power / $dx;
      my $rpratio = Math::BaseCnv::cnv($pratio,10,$radix);
      printf "  ratio %.10f [%s]\n", $ratio, $rpratio;

      $max = $value;
    }

    if ($value < $min) {
      my $dx = $dx_seq->ith($i);
      my $dy = $dy_seq->ith($i);
      my $ri = Math::BaseCnv::cnv($i,10,$radix);
      my $rdx = Math::BaseCnv::cnv($dx,10,$radix);
      my $rdy = Math::BaseCnv::cnv($dy,10,$radix);
      my $f = $dy && $dx/$dy;
      printf " min i=%d[%s] %.5f  dx=%s,dy=%s   %.3f\n", $i,$ri, $value, $dx,$dy, $f;
      $min = $value;
    }
  }

  exit 0;
}
{
  my $path = Math::PlanePath::LCornerReplicate->new;
  my ($dx,$dy) = $path->dir_maximum_dxdy;
  my $degrees = atan2($dy,$dx) * (180 / _PI);
  my $dir4 = $degrees / 360 * 4 + 4;
  print "$dx,$dy  $degrees $dir4\n";
  exit 0;
}
