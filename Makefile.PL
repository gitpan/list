use ExtUtils::MakeMaker;

WriteMakefile(
  'AUTHOR'		=> 'Dan Brumleve <dan_brumleve@yahoo.com>',
  'NAME'		=> 'list',
  'VERSION_FROM'	=> 'list.pm',
  clean			=> {FILES => '*.so'},
);
