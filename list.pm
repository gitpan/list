package list;
$VERSION = '1.0.1';

bootstrap xsub;

sub void {
  ( )
}

use xsub storeln => qw(\@$$), q{
  AV *av = (AV *)SvRV(argv[0]);
  IV k = SvIV(argv[1]);
  SV *v = argv[2];
  SvREFCNT_inc(v);
  if (av_store(av, k, v))
    SvREFCNT_inc(v);
  return v;
};

use xsub pushln => qw(\@@), q{
  AV *av = (AV *)SvRV(argv[0]);
  IV i;
  for (i = 1; i < argc; i++) {
    SV *v = argv[i];
    SvREFCNT_inc(v);
    av_push(av, v);
  }
  return newSVuv(AvFILL(av) + 1);
};

use xsub unshiftln => qw(\@@), q{
  AV *av = (AV *)SvRV(argv[0]);
  IV i;
  av_unshift(av, argc - 1);
  for (i = 1; i < argc; i++) {
    SV *v = argv[i];
    SvREFCNT_inc(v);
    if (!av_store(av, i - 1, v))
      SvREFCNT_dec(v);
  }
  return newSVuv(AvFILL(av) + 1);
};

sub ar(@) { wantarray ? @_ : \@_ }

sub reverse(@) {
  if (wantarray) {
    CORE::reverse(@_)
  } else {
    my @r;
    unshiftln @r, $_ for @_;
    \@r
  }
}

sub rotate {
  my ($a, $n) = @_;
  $n %= @$a || 1;
  ar(@$a[$n .. $#$a], @$a[0 .. $n - 1])
}

sub all(@) {
  $_ or return $_ for @_;
  1
}

sub alldef(@) {
  defined $_ or return undef for @_;
  1
}

sub any(@) {
  $_ and return $_ for @_;
  ''
}

sub anydef(@) {
  defined $_ and return $_ for @_;
  ''
}

sub min(@) : lvalue {
  my $xp = \shift;
  $_ < $$xp and $xp = \$_ for @_;
  $$xp
}

sub minstr(@) : lvalue {
  my $xp = \shift;
  $_ lt $$xp and $xp = \$_ for @_;
  $$xp
}

sub max(@) : lvalue {
  my $xp = \shift;
  $_ > $$xp and $xp = \$_ for @_;
  $$xp
}

sub maxstr(@) : lvalue {
  my $xp = \shift;
  $_ gt $$xp and $xp = \$_ for @_;
  $$xp
}

sub bound($$$) {
  my ($x, $l, $h) = @_;
  $x < $l ? $l : $x > $h ? $h : $x;
}

sub flatten(@) {
  if (wantarray) {
    map { ref($_) eq 'ARRAY' ? (@$_) : ($_) } @_
  } else {
    my @r; pushln(@r, ref($_) eq 'ARRAY' ? (@$_) : ($_)) for @_;
    \@r;
  }
}

sub sum(@) {
  my $x = 0;
  $x += $_ for @_;
  $x
}

sub product(@) {
  my $x = 1;
  $x *= $_ for @_;
  $x
}

sub mean(@) {
  @_ or return undef;
  &sum / @_
}

sub median(@) {
  @_ or return undef;
  my @x = sort { $a <=> $b } @_;
  $x[@x / 2]
}

sub phash(@) {
  my @ph = (\my %ph);
  while (@_) {
    $ph{+shift} = @ph;
    push(@ph, shift);
  }
  \@ph
}

sub phashln(@) {
  my @ph = (\my %ph);
  while (@_) {
    $ph{+shift} = @ph;
    pushln(@ph, shift);
  }
  \@ph
}

sub lfold(&@) {
  my ($f, $r) = (shift, shift);
 
  if (wantarray) {
    my @r;
    push @r, $r = $f->($r, $_) for @_;
    @r
  } else {
    $r = $f->($r, $_) for @_;
    $r
  }
}

BEGIN { *fold = *lfold }

sub rfold(&@) {
  my ($f, $r) = (shift, pop);

  if (wantarray) {
    my @r;
    unshift @r, $r = $f->($_, $r) for reverse @_;
    @r
  } else {
    $r = $f->($_, $r) for reverse @_;
    $r
  }
}

sub map2(&@) {
  my $f = shift;
  my @r;
  while (my ($k, $v) = splice(@_, 0, 2)) {
    push @r, $f->($k, $v);
  }
  @r
}

sub mapkv(&\%) {
  my ($f, $hr) = @_;

  my @r;
  while (my @kv = each(%$hr)) {
    push @r, $f->(@kv);
  }

  @r
}

sub grepkv(&\%)  {
  my ($f, $hr) = @_;
  my @r; $f->(@kv) and push @r, @kv while my @kv = each %$hr; @r
}
 
sub grep2(&@) {
  my $f = shift; my @r;
  while (my ($l, $r) = splice(@_, 0, 2)) {
    $f->($l, $r) and push @r, $l => $r;
  }
  @r
}

sub factor(&@) {
  my $f = shift; my %h; local $. = 0;
  push(@{$h{&$f}}, $_), ++$. for @_;
  @h{sort keys %h}
}

sub shift2(\@) {
  splice(@{$_[0]}, 0, 2);
}

sub pop2(\@) {
  splice(@{$_[0]}, -2, 2);
}

sub flip(@) {
  my @r;
  if (wantarray) {
    while (my ($k, $v) = \splice(@_, 0, 2)) {
      push(@r, $$v, $$k)
    }
    @r
  } else {
    while (my ($k, $v) = \splice(@_, 0, 2)) {
      pushln(@r, $$v, $$k)
    }
    \@r
  }
}

sub zip {
  my ($k, $v) = @_;
  my $m = max($#$k, $#$v);

  my @kv;
  if (wantarray) {
    push(@kv, $$k[$_], $$v[$_]) for 0 .. $m;
    @kv
  } else {
    pushln(@kv, $$k[$_], $$v[$_]) for 0 .. $m;
    \@kv
  }
}

sub unzip(@) {
  my (@k, @v, $i);
  wantarray or return [&unzipk];
  my $i = 0; push(@{($i = !$i) ? \@k : \@v}, $_) for @_;
  (\@k, \@v)
}

sub unzipln(@) {
  my (@k, @v, $i);
  wantarray or return &unzipk;
  my $i = 0; &pushln(($i = !$i) ? \@k : \@v, $_) for @_;
  (\@k, \@v)
}

sub unzipk(@) {
  my (@k, $i);
  ar @_[map $_<<1, 0..$#_>>1]
}

sub unzipv(@) {
  my (@k, $i);
  ar @_[map 1|($_<<1), 0..$#_-1>>1]
}

sub pairln(@) {
  my @r;
  push @r, ar(shift, shift) while @_;
  @r
}

sub unpair(@) : lvalue {
  my @r;
  if (wantarray) {
    push(@r, @$_[0, 1]) for @_;
    @r
  } else {
    pushln(@r, @$_[0, 1]) for @_;
    my $y = \@r; $y
  }
}

sub transpose(@) {
  my $m = max map $#$_, @_;
  my @r;

  for my $j (0 .. $m) {
    my $r = [ ];
    push @$r, $_->[$j] for @_;
    push @r, $r
  }

  @r
}

sub transposeln(@) {
  my $m = max map $#$_, @_;
  my @r;

  for my $j (0 .. $m) {
    my $r = [ ];
    pushln @$r, $_->[$j] for @_;
    push @r, $r
  }

  @r
}
   
sub cross($@) {
  my $l = shift; 
  @_ or return map [$_], @$l;

  my @m = &cross;
  map { my $i = $_; map [$i, @$_], @m } @$l
}

sub crossln($@) {
  my $l = shift; 
  @_ or return map ar($_), @$l;
  my @m = &cross;
  map { my $lp = \$_; map ar($$lp, @$_), @m } @$l
}

sub group(@) {
  my (%r, @r);
  if (wantarray) {
    while (my ($kp, $vp) = \splice(@_, 0, 2)) {
      my $k = $$kp;
      my $e = exists $r{$k};
      push(@{$r{$k}}, $$vp);
      $e or push(@r, $$kp, $r{$k});
    }
    @r
  } else {
    while (my ($kp, $vp) = \splice(@_, 0, 2)) {
      my $k = $$kp;
      my $e = exists $r{$k};
      pushln(@{$r{$k}}, $$vp);
      $e or pushln(@r, $$kp, $r{$k});
    }
    \@r
  }
}

sub regroup(@) {
  my (%r, @r);
  local (*k, *v);
  if (wantarray) {
    while ((*k, *v) = \splice(@_, 0, 2)) {
      my $e = exists $r{$k};
      push(@{$r{$k}}, ref($v) eq ARRAY ? @$v : ($v));
      $e or push(@r, $k, $r{$k});
    }
    @r
  } else {
    while ((*k, *v) = \splice(@_, 0, 2)) {
      my $e = exists $r{$k};
      pushln(@{$r{$k}}, ref($v) eq ARRAY ? @$v : ($v));
      $e or pushln(@r, $k, $r{$k});
    }
    \@r
  }
}

sub ungroup(@) {
  my @r;
  if (wantarray) {
    while (my ($kp, $vp) = \splice(@_, 0, 2)) {
      push(@r, $$kp, $_) for @$$vp;
    }
    @r
  } else {
    while (my ($kp, $vp) = \splice(@_, 0, 2)) {
      pushln(@r, $$kp, $_) for @$$vp;
    }
    \@r
  }
}

sub unique(@) : lvalue {
  my (@r, %seen);
  if (wantarray) {
    for (@_) {
      $seen{$_}++ and next;
      push(@r, $_);
    }
    @r
  } else {
    for (@_) {
      $seen{$_}++ and next;
      pushln(@r, $_);
    }
    my $y = \@r; $y
  }
}

sub first(@) : lvalue {
  $_[0]
}

sub merge(&@) {
  local $f = shift; sub _merge {
    @_ < 2 and return \@_;
    my $m = @_ >> 1;
    my $x = _merge(@_[0 .. $m-1]);
    my $y = _merge(@_[$m .. $#_]);
    ar($f->($x, $y))
  }

  &_merge
}

sub zipmerge(&@) {
  local $f = shift; sub _zipmerge {
    @_ < 2 and return \@_;
    my $x = _zipmerge(&unzipk);
    my $y = _zipmerge(&unzipv);
    ar($f->($x, $y))
  }

  &_zipmerge
}

1
