#!/usr/bin/perl

use strict;
use warnings;

use Carp qw(carp croak);
use Digest::MD5 qw(md5_hex);
use Encode qw(decode);
use File::Basename qw(dirname);
use File::HomeDir qw(my_home);
use File::Path qw(remove_tree);
use File::Slurp qw(read_file write_file);
use File::Spec qw();
use GD qw();
use Getopt::Long qw(GetOptions);
use HTML::Entities qw(decode_entities);
use JSON qw(encode_json decode_json);
use List::Util qw(min max);
use LWP::Simple qw(getstore);
use Math::Round qw(round);
use POSIX qw(ceil);
use Text::Unidecode qw(unidecode);
use TMDB qw();

$| = 1;

use constant DECADE_UNKNOWN => 32768;

my $script_dir = dirname(File::Spec->rel2abs(__FILE__));

my ($renew_info, $renew_images, $renew_labels);
my %opt = (
		'renew-details' => \$renew_info,
		'renew-images' => \$renew_images,
		'renew-labels' => \$renew_labels
	);
my %config = (
		alphabet_boundaries => 'IR',
		regular_font_file => $script_dir . '/Cabin-Regular.otf',
		bold_font_file => $script_dir . '/Cabin-Bold.otf',
		small_font_size => 17,
		normal_font_size => 19,
		large_font_size => 22,
		larger_font_size => 26,
		leading => 1.5,
		paragraph_spacing => 0.5,
		max_stars => 3,
		backdrop_alpha => .075,
		background_width => 1920,
		background_height => 1080,
		background_padding => 50,
		trail_height => 40,
		scrollbar_width => 40,
		icon_width => 800,
		icon_height => 400,
		poster_width => 240,
		poster_height => 360,
		jpeg_quality => 90,
		cache_max_age => 30 * 24 * 60 * 60,
		data_dir_name => '__Dune',
		access_protocol => 'nfs',
		smb_username => undef,
		smb_password => undef
	);
# TODO More robust option handling
foreach my $key (keys %config) {
	(my $option_name = $key) =~ s/_/-/g;
	my $type = (defined $config{$key} && $config{$key} =~ /^[0-9.]+$/)
		? 'f' : 's';
	$opt{"$option_name=$type"} = \$config{$key};
}
$opt{'ignore-genre=s'} = $config{ignore_genres} = [];
$opt{'filename-ignore=s'} = $config{filename_ignore} = [];
$opt{'video-file-extension=s'} = $config{video_file_extensions} = [];
GetOptions(%opt) or exit 1;
$config{alphabet_boundaries} = [ split(//, $config{alphabet_boundaries}) ];
@{$config{video_file_extensions}} = qw(avi mkv mp4 m4v mpg wmv mov flv vob)
	unless @{$config{video_file_extensions}};

my $config_file = my_home() . '/.dune-movie-prettifier.json';
if (-f $config_file) {
	my $conf = read_json($config_file);
	while (my ($key, $value) = each %$conf) {
		$config{$key} = $value if exists $config{$key};
	}
}

@ARGV < 2 and croak "Usage: $0 server share [mount]";
my ($server_name, $share_name, $base_path) = @ARGV;
$base_path = "//$server_name/$share_name" unless defined $base_path;

my $base_url;
if ($config{access_protocol} eq 'smb') {
	my $auth;
	if (defined $config{smb_username} && $config{smb_username} ne '') {
		require URI::Escape;
		$auth = URI::Escape::uri_escape($config{smb_username});
		$auth .= ':' . URI::Escape::uri_escape($config{smb_password})
			if defined $config{smb_password} && $config{smb_password} ne '';
		$auth .= '@';
	} else {
		$auth = '';
	}
	$base_url = "$config{access_protocol}://$auth$server_name/$share_name";
} else {
	# NFS assumed
	$base_url = "$config{access_protocol}://$server_name:/$share_name";
}

my $dune_path = $base_path . '/' . $config{data_dir_name};
my $dune_url = $base_url . '/' . $config{data_dir_name};

my $api_key_file = my_home() . '/.tmdb';
open(my $afh, '<', $api_key_file)
	or croak qq{Cannot open "$api_key_file" for reading: $!};
my $api_key = <$afh>;
close($afh);
chomp $api_key;

my $movie_ignore_re = join '|', '(?:19|20)\d\d', @{$config{filename_ignore}};
my %ignore_genre = map { $_ => undef } @{$config{ignore_genres}};
my %font_metrics = ();

attempt_mkdir("$dune_path");
attempt_mkdir("$dune_path/labels");
attempt_mkdir("$dune_path/menu");
attempt_mkdir("$dune_path/content");
attempt_mkdir("$dune_path/cache");
attempt_mkdir("$dune_path/cache/mediainfo");
attempt_mkdir("$dune_path/cache/tmdb");
attempt_mkdir("$dune_path/cache/backdrops");
attempt_mkdir("$dune_path/cache/posters");

my %movie_ids = ();
my $id_file = "$dune_path/cache/tmdb/id.txt";
if (-e $id_file) {
	open(my $fh, '<', $id_file)
		or croak qq{Cannot open "$id_file" for reading: $!};
	while (<$fh>) {
		/(.*?)=(.*)/ and $movie_ids{$1} = $2;
	}
	close($fh);
}

my $tmdb = TMDB->new('apikey' => $api_key);

opendir(my $root_dh, $base_path)
	or croak qq{Cannot open directory "$base_path": $!};
my @movies = readdir($root_dh);
closedir($root_dh);

@movies = sort { uc($a) cmp uc($b) }
	grep { /^[^._]/ && -d "$base_path/$_" } @movies;

foreach my $movie_id (grep { !exists $movie_ids{$_} } @movies) {
	print "Determining TMDB ID of $movie_id ...\n";
	(my $query = $movie_id) =~ s/[.!,;\-_]/ /g;
	$query =~ s/\ (?:$movie_ignore_re)(?:\ .*|$)//oix;
	my $tmdb_id;
	my @candidates = $tmdb->search->movie($query);
	if (@candidates == 0) {
		print STDERR "No results!\n";
		print 'Enter TMDB ID: ';
		$tmdb_id = <STDIN>;
		$tmdb_id =~ s/\s+//s;
		$tmdb_id = undef unless $tmdb_id;
	} elsif (@candidates == 1) {
		$tmdb_id = $candidates[0]{id};
	} else {
		my $len = length(scalar @candidates);
		for (my $i = 0; $i < @candidates; $i++) {
			printf "[%${len}d] %s (%s) (%s)\n", $i + 1,
				$candidates[$i]{title},
				(defined $candidates[$i]{release_date}
					? $candidates[$i]{release_date}
					: '?'),
				$candidates[$i]{id};
		}
		print 'Choice (or +TMDB ID): ';
		my $sel = <STDIN>;
		$sel =~ s/\s+//s;
		$tmdb_id = ($sel =~ /^\+(\d+)$/
			? $1
			: ($sel > 0 && $sel <= @candidates
				? $candidates[$sel - 1]{id}
				: undef));
	}
	if (defined $tmdb_id) {
		print "ID is $tmdb_id\n";
		$movie_ids{$movie_id} = $tmdb_id;
	} else {
		print "ID unknown\n";
	}
}

my %temp = map { $_ => undef } @movies;
foreach my $m (grep { !exists $temp{$_} } keys %movie_ids) {
	print "Cached movie $m no longer exists\n";
	delete $movie_ids{$m};
	unlink "$dune_path/cache/backdrops/$m.jpg",
		"$dune_path/cache/posters/$m.jpg";
	remove_tree "$dune_path/content/$m";
}

open(my $fh, '>:utf8', $id_file)
	or croak qq{Cannot open "$id_file" for writing: $!};
while (my ($movie_id, $id) = each %movie_ids) {
	print $fh "$movie_id=$id\n";
}
close($fh);

my %by_alpha = ();
my %by_genre = ();
my %by_decade = ();
my %by_actor = ();
my %by_director = ();
my %by_writer = ();

foreach my $movie_id (sort { uc($a) cmp uc($b) } keys %movie_ids) {
	my $id = $movie_ids{$movie_id};
	print "$movie_id ($id)\n";

	my $cache_file = "$dune_path/cache/tmdb/$id.json";
	my $backdrop_filename = "$dune_path/cache/backdrops/$movie_id.jpg";
	my $poster_filename = "$dune_path/cache/posters/$movie_id.jpg";

	my $renew = $renew_info || !-e $cache_file
		|| (stat($cache_file))[9] + $config{cache_max_age} < time;
	my $data;
	if ($renew) {
		my $movie = $tmdb->movie('id' => $id);
		my $info = $movie->info;
		until (defined $info) {
			sleep 1;
			$info = $movie->info;
		}
		my $title = $info->{original_title};
		$title = decode('utf8',
			defined $title && $title ne '' ? $title : $info->{title});
		my $year = $info->{release_date};
		$year =~ s/-.*// if defined $year;
		my $runtime = $info->{runtime};
		my $summary = decode('utf8', $info->{overview});
		my @genres = $info->{genres}
			? sort_nicely(
				grep { !exists $ignore_genre{$_} }
				map { decode('utf8', $_->{name}); }
				@{$info->{genres}})
			: ();
		# XXX ->_cast is hidden, but saves a call and doesn't lose ->{order}
		my $castcrew = $movie->_cast;
		my @stars = remove_duplicates(
			map { decode('utf8', $_->{name}) }
			sort { $a->{order} <=> $b->{order} }
			@{$castcrew->{cast}});
		splice @stars, $config{max_stars} if @stars > $config{max_stars};
		my @directors = remove_duplicates(
			map { decode('utf8', $_->{name}) }
			grep { $_->{department} eq 'Directing' && $_->{job} eq 'Director' }
			@{$castcrew->{crew}});
		my @writers = remove_duplicates(
			map { decode('utf8', $_->{name}) }
			grep { $_->{department} eq 'Writing'
				&& ($_->{job} eq 'Screenplay' || $_->{job} eq 'Writer') }
			@{$castcrew->{crew}});
		$data = {
				'title' => $title,
				'year' => $year,
				'runtime' => $runtime,
				'summary' => $summary,
				'genres' => \@genres,
				'stars' => \@stars,
				'writers' => \@writers,
				'directors' => \@directors
			};
		write_json($data, $cache_file);

		my %imgs = (
				'poster' => $poster_filename,
				'backdrop' => $backdrop_filename
			);
		while (my ($type, $path) = each %imgs) {
			unlink $path;
			next unless defined $info->{"${type}_path"};
			my $url = $tmdb->config->img_base_url . 'original'
				. $info->{"${type}_path"};
			getstore($url, $path);
		}
	} else {
		$data = read_json($cache_file);
	}
	
	my $sanitized = uc(strip_article(unaccent($data->{title})));
	my $record = [ strip_non_words($sanitized), $movie_id ];
	
	my $first = substr $sanitized, 0, 1;
	my $aid;
	if ($first ge 'A' && $first le 'Z') {
		$aid = 0;
		while ($aid < @{$config{alphabet_boundaries}}
				&& $first ge $config{alphabet_boundaries}->[$aid]) {
			$aid++;
		}
	} else {
		$aid = -1;
	}
	push @{$by_alpha{$aid}}, $record;
	my $decade = $data->{year} ? int($data->{year} / 10) : DECADE_UNKNOWN;
	push @{$by_decade{$decade}}, $record;
	foreach my $genre (@{$data->{genres}}) {
		push @{$by_genre{$genre}}, $record;
	}	
	foreach my $actor (@{$data->{stars}}) {
		push @{$by_actor{$actor}}, $record;
	}
	foreach my $director (@{$data->{directors}}) {
		push @{$by_director{$director}}, $record;
	}
	foreach my $writer (@{$data->{writers}}) {
		push @{$by_writer{$writer}}, $record;
	}

	my $movie_dir = "$dune_path/content/$movie_id";
	next if !$renew_images && !$renew && -e $movie_dir;
	attempt_mkdir($movie_dir);

	my $padding = ($config{icon_height} - $config{poster_height}) / 2;
	my $icon_gd = GD::Image->new($config{icon_width}, $config{icon_height}, 1);
	my $black = $icon_gd->colorAllocate(0, 0, 0);
	my $white = $icon_gd->colorAllocate(255, 255, 255);
	my $transp = $icon_gd->colorAllocateAlpha(255, 255, 255, 79);
	$poster_filename = "$dune_path/default_poster.jpg"
		unless -s $poster_filename;
	my $head;
	open(my $pfh, '<', $poster_filename)
		or croak qq{Cannot open "$poster_filename" for reading: $!};
	read $pfh, $head, 4;
	close $pfh;
	my $poster_gd = $head eq chr(hex(89)) . 'PNG'
		? GD::Image->newFromPng($poster_filename, 1)
		: GD::Image->newFromJpeg($poster_filename, 1);
	my $par = $poster_gd->width / $poster_gd->height;
	my $iar = $config{poster_width} / $config{poster_height};
	my ($srcx, $srcy, $srcw, $srch);
	if ($par < $iar) {
		$srcx = 0;
		$srcw = $poster_gd->width;
		my $dar = $iar - $par;
		$srcy = $poster_gd->height * $dar / 2;
		$srch = $poster_gd->height * (1 - $dar);
	} else {
		$srcy = 0;
		$srch = $poster_gd->height;
		my $dar = $par - $iar;
		$srcx = $poster_gd->width * $dar / 2;
		$srcw = $poster_gd->width * (1 - $dar);
	}
	$icon_gd->copyResampled($poster_gd,
		$padding, $padding, $srcx, $srcy, $config{poster_width}, $config{poster_height},
		$srcw, $srch);
	
	my $byline;
	if ($data->{year} || $data->{title}) {
		if ($data->{year}) {
			$byline = ($data->{runtime}
				? "$data->{year} &ndash; $data->{runtime} min" : $data->{year});
		} else {
			$byline = "$data->{runtime} min";
		}
	}

	my @paragraphs = ();
	push @paragraphs, [ $data->{title}, 1 ];
	push @paragraphs, [ $byline, 0 ]
		if defined $byline;
	push @paragraphs, [ join(', ',
			map { s/ /&nbsp;/g; $_; } @{$data->{genres}}), 0 ]
		if @{$data->{genres}};
	push @paragraphs, [ nice_array(@{$data->{stars}}), 0 ]
		if @{$data->{stars}};
	
	my $m = get_font_metrics($config{regular_font_file}, $config{normal_font_size});
	my $pad = $config{paragraph_spacing} * $m->{line_height};

	my $m2 = get_font_metrics($config{bold_font_file}, $config{larger_font_size});
	my $x = $padding + $config{poster_width}
		+ ($config{leading} - 1 + $config{paragraph_spacing}) * $m2->{line_height};
	my $w = $config{icon_width} - $padding - $x;
	my $y = $padding;
	my ($font_file, $font_size, $color);
	foreach my $par (@paragraphs) {
		if ($par->[1]) {
			($font_file, $font_size, $color)
				= ($config{bold_font_file}, $config{larger_font_size}, $white);
		} else {
			($font_file, $font_size, $color)
				= ($config{regular_font_file}, $config{normal_font_size}, $transp);
		}
		$y = render_text($icon_gd, $par->[0], $x, $y, $w, undef,
			$font_file, $font_size, $config{leading}, $color);
		$y += $pad;
	}
	
	write_image($icon_gd, "$dune_path/content/$movie_id/icon.png");
	
	my $bg_padding = 100;
	my $bg_poster_text_dist = 100;
	my $text_margin = 100;
	my $bgph = $config{background_height} - 2 * $bg_padding;
	my $bgpw = $poster_gd->width * $bgph / $poster_gd->height;
	my $bg_gd = GD::Image->new($config{background_width}, $config{background_height}, 1);
	$black = $bg_gd->colorAllocate(0, 0, 0);
	$white = $bg_gd->colorAllocate(255, 255, 255);
	$transp = $bg_gd->colorAllocateAlpha(255, 255, 255, 79);
	if (-s $backdrop_filename) {
		my $bd_gd = GD::Image->newFromJpeg($backdrop_filename, 1);
		my $tb = $bg_gd->colorAllocateAlpha(
			0, 0, 0, round(127 * $config{backdrop_alpha}));
		$bd_gd->filledRectangle(0, 0, $bd_gd->width - 1, $bd_gd->height - 1, $tb);
		$bg_gd->copyResampled($bd_gd, 0, 0, 0, 0,
			$config{background_width}, $config{background_height}, $bd_gd->width, $bd_gd->height);
	}
	$bg_gd->copyResampled($poster_gd,
		$bg_padding, $bg_padding, 0, 0, $bgpw, $bgph,
		$poster_gd->width, $poster_gd->height);
	$x = $bg_padding + $bgpw + $bg_poster_text_dist;
	$w = $config{background_width} - $x - $bg_padding;
	
	my @byline = ();
	push @byline, $byline if defined $byline;
	push @byline, join(', ', map { s/ /&nbsp;/g; $_; } @{$data->{genres}})
		if @{$data->{genres}};

	@paragraphs = ();
	push @paragraphs, [ $data->{title}, 1 ];
	push @paragraphs, [ join(' &ndash; ', @byline), 0 ]
		if @byline;
	if (@{$data->{writers}} && @{$data->{directors}}) {
		my $writers = nice_array(@{$data->{writers}});
		my $directors = nice_array(@{$data->{directors}});
		if ($writers eq $directors) {
			push @paragraphs, [ "Written and directed by $writers", 0 ];
		} else {
			push @paragraphs, [ "Written by $writers", 0 ];
			push @paragraphs, [ "Directed by $directors", 0 ]
		}
	} elsif (@{$data->{writers}}) {
		my $writers = nice_array(@{$data->{writers}});
		push @paragraphs, [ "Written by $writers", 0 ];
	} elsif (@{$data->{directors}}) {
		my $directors = nice_array(@{$data->{directors}});
		push @paragraphs, [ "Directed by $directors", 0 ];
	}
	push @paragraphs, [ 'Starring ' . nice_array(@{$data->{stars}}), 0 ]
		if @{$data->{stars}};
	if (defined $data->{runtime}) {
		push @paragraphs, undef;
		push @paragraphs, [ $data->{summary}, -1 ];
	}

	my $playw = 300;
	my $playh = 50;
	my $playx = $config{background_width} - $bg_padding - $playw;
	my $playy = $config{background_height} - $bg_padding - $text_margin - $playh;

	$m = get_font_metrics($config{regular_font_file}, $config{small_font_size});
	$y = $bg_padding + $text_margin;
	PAR: foreach my $par (@paragraphs) {
		unless (defined $par) {
			$m = get_font_metrics($font_file, $font_size);
			$y += $config{paragraph_spacing} * $m->{line_height};
			next;
		}
		if ($par->[1] == -1) {
			($font_file, $font_size, $color)
				= ($config{regular_font_file}, $config{small_font_size}, $transp);
			$color = $transp;
		} elsif ($par->[1] == 1) {
			($font_file, $font_size, $color)
				= ($config{bold_font_file}, $config{larger_font_size}, $white);
		} else {
			($font_file, $font_size, $color)
				= ($config{regular_font_file}, $config{normal_font_size}, $transp);
		}
		my $maxh = $playy - $y - $m->{line_height};
		$y = render_text($bg_gd, $par->[0], $x, $y, $w, $maxh,
			$font_file, $font_size, $config{leading}, $color);
		$m = get_font_metrics($font_file, $font_size);
		$y += $config{paragraph_spacing} * $m->{line_height};
	}

	my $dir = "$base_path/$movie_id";
	my $sub = (-d "$dir/VIDEO_TS") ? '/VIDEO_TS' : '';
	opendir(my $movie_dh, "$dir$sub")
		or croak qq{Cannot open directory "$dir$sub": $!};
	my @movie_files = readdir($movie_dh);
	closedir($movie_dh);
	my $ext_re = join '|', @{$config{video_file_extensions}};
	@movie_files = sort_nicely(grep { /\.(?:$ext_re)$/i } @movie_files);
	
	if (@movie_files > 1) {
		my $playlist_file = "$dune_path/content/$movie_id/playlist.pls";
		open(my $pls, '>:utf8', $playlist_file)
			or croak qq{Cannot open "$playlist_file" for writing: $!};
		print $pls "[playlist]\n";
		my $cnt = 0;
		foreach my $file (@movie_files) {
			printf $pls "File%d=%s\n", ++$cnt, "$base_url/$movie_id$sub/$file";
		}
		printf $pls "NumberOfEntries=%d\n", $cnt;
		print $pls "Version=2\n";
		close($pls);
	}
	
	my $mediainfo = get_media_info(
		[ map { qq|"$dir$sub/$_"| } @movie_files ], $movie_id);
	#my $bitrate = $mediainfo->{size} / $mediainfo->{duration}
	#	* 8 * 1000 / 1024 / 1024; # bits, ms, kb, mb -> Mbps
	my $ar = $mediainfo->{video}{width} / $mediainfo->{video}{height};
	my $sar = $config{background_width} / $config{background_height};
	my $scaled_height = ($ar < $sar)
		? $mediainfo->{video}{height}
		: $mediainfo->{video}{width} / $sar;
	my @res = qw(1080 720 576 480);
	my $i = 0;
	while ($i < @res && $scaled_height < $res[$i]) {
		$i++;
	}
	my $res = ($i < @res)
		? $res[$i] . 'p'
		: $mediainfo->{video}{width} . '×' . $mediainfo->{video}{height};
	my $sep = ' ' x 4;
	my $mediainfo_line = sprintf
		"%s${sep}%s${sep}%s fps${sep}%s",
		$mediainfo->{video}{ar},
		$res,
		$mediainfo->{video}{fps},
		$mediainfo->{audio}{codec};
	# XXX Can't use duration for display, since it might include extras etc.

	my $darken = $bg_gd->colorAllocateAlpha(0, 0, 0, 63);
	$bg_gd->filledRectangle($playx, $playy,
		$playx + $playw - 1, $playy + $playh - 1, $darken);
	$bg_gd->rectangle($playx, $playy,
		$playx + $playw - 1, $playy + $playh - 1, $white);
	$m = get_font_metrics($config{regular_font_file}, $config{normal_font_size});
	my $line_height = $m->{ascent};
	$bg_gd->stringFT($transp, $config{regular_font_file}, $config{normal_font_size}, 0,
		$x,
		$playy + ($playh - $line_height) / 2 + $line_height * .85,
		$mediainfo_line);
	my $label = 'Play Movie';
	my $width = (GD::Image->stringFT($white,
		$config{bold_font_file}, $config{normal_font_size}, 0, 0, 0, $label))[2];
	$bg_gd->stringFT($white, $config{bold_font_file}, $config{normal_font_size}, 0,
		$playx + ($playw - $width) / 2,
		$playy + ($playh - $line_height) / 2 + $line_height * .85,
		$label);
	
	my $file = @movie_files > 1
		? "$dune_url/content/$movie_id/playlist.pls"
		: "$base_url/$movie_id$sub/$movie_files[0]";
	my $folder_file = "$dune_path/content/$movie_id/dune_folder.txt";
	open(my $movie_fh, '>:utf8', $folder_file)
		or croak qq{Cannot open "$folder_file" for writing: $!};
	print $movie_fh <<END;
system_files = *
sort_field = unsorted
animation_enabled = no
use_icon_view = yes
num_cols = 1
num_rows = 1
paint_path_box = no
paint_scrollbar = no
paint_captions = no
paint_help_line = no
paint_icon_selection_box = no
paint_content_box_background = no
direct_children.icon_valign = center
background_path = $dune_url/content/$movie_id/background.png
background_x = 0
background_y = 0
content_box_x = $playx
content_box_y = $playy
content_box_width = $playw
content_box_height = $playh
item.0.caption = $data->{title}
item.0.icon_path = -
item.0.media_url = $file
END
	close($movie_fh);

	write_image($bg_gd,
		"$dune_path/content/$movie_id/background.png",
		1);
}

my $rowh = 70;
my $fix = 40;

my $cboxw = $config{background_width} - 2 * $config{background_padding};
my $cboxwx = $cboxw + $config{scrollbar_width};
my $bodyh = $config{background_height} - 2 * $config{background_padding} - $config{trail_height};

my $maxrowcnt = int(($bodyh - $fix) / $rowh);

my $cboxx = $config{background_padding};
my $cboxh = $rowh + $fix;
my $cboxy = $config{background_padding} + int(($bodyh - $cboxh) / 2);

my (@keys, %data);

@keys = ();
%data = ();
foreach my $did (sort { $a <=> $b } keys %by_decade) {
	push @keys, $did;
	my $caption = $did == DECADE_UNKNOWN ? 'Unknown' : $did . '0s';
	$data{$did} = [ $caption, $by_decade{$did} ];
}
create_movie_folders('decade', \@keys, \%data, 9, 1);

@keys = ();
%data = ();
my @letters = (@{$config{alphabet_boundaries}}, chr(ord('Z') + 1));
my $first_letter = 'A';
for (my $i = 0; $i < @letters; $i++) {
	my $second_letter = chr(ord($letters[$i]) - 1);
	if ($by_alpha{$i}) {
		my $label = "$first_letter&ndash;$second_letter";
		push @keys, lc $first_letter;
		$data{lc $first_letter} = [ $label, $by_alpha{$i} ];
	}
	$first_letter = $letters[$i];
}
if ($by_alpha{-1}) {
	push @keys, '_';
	$data{'_'} = [ 'Other', $by_alpha{-1} ];
}
create_movie_folders('title', \@keys, \%data, scalar(@keys), 1);

my %d = ('genre' => \%by_genre, 'star' => \%by_actor,
	'director' => \%by_director, 'writer' => \%by_writer);
while (my ($type, $info) = each %d) {
	@keys = ();
	%data = ();
	foreach my $name (sort_nicely(keys %$info)) {
		my $hash = md5_hex($name);
		push @keys, $hash;
		$data{$hash} = [ $name, $info->{$name} ];
	}
	create_movie_folders($type, \@keys, \%data, 5, $type eq 'genre');
}

print "Creating overview ...", $/;
create_background('menu', []);
open(my $root_fh, '>:utf8', "$base_path/dune_folder.txt")
	or croak qq{Cannot open "$base_path/dune_folder.txt" for writing: $!};
print $root_fh <<END;
system_files = *
sort_field = unsorted
animation_enabled = no
use_icon_view = yes
num_cols = 6
num_rows = 1
paint_captions = no
paint_icons = yes
paint_path_box = no
paint_help_line = no
paint_icon_selection_box = yes
paint_content_box_background = yes
direct_children.icon_valign = center
background_path = $dune_url/menu/background.png
background_x = 0
background_y = 0
content_box_x = $cboxx
content_box_y = $cboxy
content_box_width = $cboxwx
content_box_height = $cboxh
END
my $i = 0;
foreach my $by (qw(title genre decade star director writer)) {
	my $d = ucfirst $by;
	my $caption = "By $d";
	my $hash = create_label($caption,
		$config{regular_font_file}, $config{large_font_size});
	print $root_fh <<END;
item.$i.caption = $caption
item.$i.icon_path = $dune_url/labels/$hash.png
item.$i.media_url = $dune_url/menu/$by
item.$i.media_action = browse
END
	$i++;
}
close($root_fh);
print "Done.", $/;

sub get_media_info {
	my ($movie_files, $movie_id) = @_;
	my $info_file = "$dune_path/cache/mediainfo/$movie_id.json";
	if (-e $info_file) {
		return read_json($info_file);
	}
	my ($codec, $acodec, $vcodec, $vwidth, $vheight, $var, $vfps);
	my $filesize = 0;
	my $duration = 0;
	my $sect;
	my $new_sect = 1;
	my $new_size = 0;
	my $new_duration = 0;
	my $cmd = 'mediainfo --full ' . join ' ', @$movie_files;
	open(my $ph, '-|', $cmd)
		or croak qq{Cannot invoke mediainfo: $!};
	while (my $line = <$ph>) {
		chomp $line;
		if ($line eq '') {
			$new_sect = 1;
		} elsif ($new_sect) {
			$new_sect = 0;
			($sect = $line) =~ s/\s+#\d+$//;
			$new_size = $new_duration = 1 if $sect eq 'General';
		} elsif ($line =~ /^(.*?)\s*:\s*(.*?)\s*$/) {
			my ($key, $value) = ($1, $2);
			if ($sect eq 'General' && $key eq 'Format') {
				$codec = $value unless defined $codec;
			} elsif ($sect eq 'General' && $key eq 'File size') {
				if ($new_size && $value =~ /^\d+$/) {
					$filesize += $value;
					$new_size = 0;
				}
			} elsif ($sect eq 'General' && $key eq 'Duration') {
				if ($new_duration && $value =~ /^\d+$/) {
					$duration += $value;
					$new_duration = 0;
				}
			} elsif ($sect eq 'Video' && $key eq 'Format') {
				$vcodec = $value unless defined $vcodec;
			} elsif ($sect eq 'Video' && $key eq 'Width') {
				$vwidth = $value unless defined $vwidth;
			} elsif ($sect eq 'Video' && $key eq 'Height') {
				$vheight = $value unless defined $vheight;
			} elsif ($sect eq 'Video' && $key eq 'Display aspect ratio') {
				$var = $value if !defined $var
					|| ($var !~ /:/ && $value =~ /:/);
			} elsif ($sect eq 'Video' && $key eq 'Frame rate') {
				$vfps = $value unless defined $vfps;
			} elsif ($sect eq 'Audio'
					&& ($key eq 'Codec ID/Hint' || $key eq 'Codec')) {
				$acodec = $value unless defined $acodec;
			}
		}
	}
	close($ph);
	$vfps =~ s/\.?0+$//;
	$var = sprintf '%.2f:1', $var unless $var =~ /:/;
	my $mediainfo = {
			'codec' => $codec,
			'duration' => $duration,
			'size' => $filesize,
			'video' => {
				'codec'=> $vcodec,
				'width' => $vwidth,
				'height' => $vheight,
				'fps' => $vfps,
				'ar' => $var
			},
			'audio' => {
				'codec' => $acodec
			}
		};
	write_json($mediainfo, $info_file);
	return $mediainfo;
}

sub create_movie_folders {
	my ($name, $keys, $data, $cols, $large) = @_;
	print "Creating listings per $name ...", $/;
	my $rel = "menu/$name";
	my $abs = "$dune_path/$rel";
	my $trail = [ 'By ' . ucfirst($name) ];
	attempt_mkdir($abs);
	create_background($rel, $trail);
	my $rows = ceil(@keys / $cols);
	my $rowcnt = min $maxrowcnt, $rows;
	my $colcnt = min $cols, scalar @keys;
	my $cboxw = $rows > $rowcnt ? $cboxw : $cboxwx;
	my $cboxh = $rowcnt * $rowh + $fix;
	my $cboxx = $config{background_padding};
	my $cboxy = $config{background_padding} + int(($bodyh - $cboxh) / 2);
	open(my $fh, '>:utf8', "$abs/dune_folder.txt")
		or croak qq{Cannot open "$abs/dune_folder.txt" for writing: $!};
	print $fh <<END;
system_files = *
sort_field = unsorted
animation_enabled = no
use_icon_view = yes
num_rows = $rowcnt
num_cols = $colcnt
paint_captions = no
paint_icons = yes
paint_path_box = no
paint_help_line = no
paint_icon_selection_box = yes
paint_content_box_background = yes
direct_children.icon_valign = center
background_path = $dune_url/$rel/background.png
background_x = 0
background_y = 0
content_box_x = $cboxx
content_box_y = $cboxy
content_box_width = $cboxw
content_box_height = $cboxh
END
	my $cnt = 0;
	foreach my $key (@keys) {
		my ($caption, $keydata) = @{$data->{$key}};
		create_movie_folder("$rel/$key", $keydata, [ @$trail, $caption ]);
		my $hash = create_label($caption,
			$config{regular_font_file},
			$large ? $config{large_font_size} : $config{normal_font_size},
			scalar(@$keydata),
			$config{regular_font_file},
			$config{small_font_size});
		$caption = decode_entities($caption);
		print $fh <<END;
item.$cnt.caption = $caption
item.$cnt.icon_path = $dune_url/labels/$hash.png
item.$cnt.media_url = $dune_url/menu/$name/$key
item.$cnt.media_action = browse
END
		$cnt++;
	}
	close($fh);
}

sub create_movie_folder {
	my ($rel, $movies, $trail) = @_;
	my $abs = "$dune_path/$rel";
	attempt_mkdir($abs);
	create_background($rel, $trail);
	my $w = @$movies > 4 ? $cboxw : $cboxwx;
	my $cboxx = $config{background_padding};
	my $cboxy = $config{background_padding};
	my $cboxh = $bodyh;
	open(my $fh, '>:utf8', "$abs/dune_folder.txt")
		or croak qq{Cannot open "$abs/dune_folder.txt" for writing: $!};
	print $fh <<END;
system_files = *
sort_field = unsorted
animation_enabled = no
use_icon_view = yes
num_cols = 2
num_rows = 2
paint_captions = no
paint_path_box = no
paint_help_line = no
paint_icon_selection_box = yes
paint_content_box_background = no
direct_children.icon_valign = center
background_path = $dune_url/$rel/background.png
background_x = 0
background_y = 0
content_box_x = $cboxx
content_box_y = $cboxy
content_box_width = $w
content_box_height = $cboxh
END
	my $rcnt = 0;
	foreach my $entry (sort { $a->[0] cmp $b->[0] } @$movies) {
		printf $fh <<END;
item.$rcnt.caption = -
item.$rcnt.icon_path = $dune_url/content/$entry->[1]/icon.png
item.$rcnt.media_url = $dune_url/content/$entry->[1]
item.$rcnt.media_action = browse
END
		$rcnt++;
	}
	close($fh);
}

sub create_background {
	my ($rel, $trail) = @_;
	my $abs = "$dune_path/$rel";
	my $file = "$abs/background.png";
	return if !$renew_images && -e $file;
	my $img = GD::Image->new($config{background_width}, $config{background_height}, 1);
	$img->colorAllocate(0, 0, 0);
	my $fg = $img->colorAllocateAlpha(255, 255, 255, 79);
	my $text = join('  >  ', 'Home', 'Movies', @$trail);
	my $font_file = $config{regular_font_file};
	my $font_size = $config{small_font_size};
	my $m = get_font_metrics($font_file, $font_size);
	my $w = get_text_width($text, $font_file, $font_size);
	my $x = ($config{background_width} - $w) / 2;
	my $y = $config{background_height} - $config{background_padding};
	$img->stringFT($fg, $font_file, $font_size, 0, $x, $y, $text);
	write_image($img, $file);
}

sub create_label {
	my ($str, $font_file, $font_size, $str2, $font_file2, $font_size2) = @_;
	my $hash = md5_hex(join("\0", @_));
	my $file = "$dune_path/labels/$hash.png";
	if ($renew_labels || !-s $file) {
		my $padding = 20;
		my $tw = get_text_width($str, $font_file, $font_size);
		my $m = get_font_metrics($font_file, $font_size);
		my $th = $m->{ascent};
		my ($tw2, $th2, $sp);
		if (defined $str2) {
			$tw2 = get_text_width($str2, $font_file2, $font_size2);
			$sp = get_text_width('x', $font_file, $font_size);
			$m = get_font_metrics($font_file2, $font_size2);
			$th2 = $m->{ascent};
		} else {
			$tw2 = $th2 = $sp = 0;
		}
		my $w = $padding + $tw + $sp + $tw2 + $padding;
		my $h = $padding + max($th, $th2) + $padding;
		my $img = GD::Image->new($w, $h, 1);
		$img->saveAlpha(1);
		$img->alphaBlending(0);
		my $black = $img->colorAllocateAlpha(0, 0, 0, 127);
		$img->fill(0, 0, $black);
		my $white = $img->colorAllocate(255, 255, 255);
		my $x = ($w - $tw) / 2;
		my $y = ($h + $th) / 2;
		$img->stringFT($white, $font_file, $font_size, 0, $x, $y, $str);
		if (defined $str2) {
			my $transp = $img->colorAllocateAlpha(255, 255, 255, 79);
			$x += $tw + $sp;
			$img->stringFT($transp, $font_file2, $font_size2, 0, $x, $y, $str2);
		}
		write_image($img, $file);
	}
	return $hash;
}

sub render_text {
	my ($img, $text, $x1, $y1, $width, $max_height,
		$font_file, $font_size, $leading,
		$font_color, $background_color, $padding) = @_;
	return unless defined $text && length $text;
	$padding = 0 unless defined $padding;
	my @words = split /\s/, $text;
	(my $line = shift @words) =~ s/&nbsp;/ /g;
	my @lines = ();
	my $text_width = $width - 2 * $padding;
	my $max_text_height = (defined $max_height ? $max_height : $img->height)
		- 2 * $padding;
	my $text_height = 0;
	my $metrics = get_font_metrics($font_file, $font_size);
	my $next_text_height = $metrics->{ascent};
	my $line_height = $metrics->{line_height};
	while (@words && $next_text_height <= $max_text_height) {
		(my $word = shift @words) =~ s/&nbsp;/ /g;
		my $new = "$line $word";
		my $w = get_text_width($new, $font_file, $font_size);
		if ($w > $text_width) {
			my $next_next = $next_text_height + $line_height * $leading;
			if ($next_next > $max_text_height) {
				$line = trim_line($line, $text_width,
					$font_file, $font_size, $font_color);
			}
			push @lines, $line;
			$line = $word;
			$text_height = $next_text_height;
			$next_text_height = $next_next;
		} else {
			$line = $new;
		}
	}
	if ($next_text_height <= $max_text_height) {
		push @lines, $line;
		$text_height = $next_text_height;
	}
	if (defined $background_color) {
		my $x2 = $x1 + $width - 1;
		my $y2 = $y1 + $padding + $text_height + $padding - 1;
		$img->filledRectangle($x1, $y1, $x2, $y2, $background_color);
	}
	my $y = $y1 + $padding;
	foreach my $line (@lines) {
		$img->stringFT($font_color, $font_file, $font_size, 0,
			$x1 + $padding, $y + $metrics->{ascent}, $line);
		$y += $line_height * $leading;
	}
	$y += $padding;
	return $y;
}

sub get_font_metrics {
	my ($font_file, $font_size) = @_;
	my $key = "${font_file}_$font_size";
	if (!exists $font_metrics{$key}) {
		my $c = 'X'; # Works best for ascent
		my @bounds = GD::Image->stringFT(
			0, $font_file, $font_size, 0, 0, 0, "$c\r\n$c");
		my $h = $bounds[1] - $bounds[7];
		@bounds = GD::Image->stringFT(
			0, $font_file, $font_size, 0, 0, 0, $c);
		my $ch = $bounds[1] - $bounds[7];
		$font_metrics{$key} = {
				'line_height' => $h - $ch,
				'ascent' => $ch
			};
	}
	return $font_metrics{$key};
}

sub get_text_width {
	my ($text, $font_file, $font_size) = @_;
	return (GD::Image->stringFT(0, $font_file, $font_size, 0, 0, 0, $text))[2];
}

sub trim_line {
	my ($line, $text_width, $font_file, $font_size, $font_color) = @_;
	$line =~ s/[.,;:?!\- ]*$//;
	my $width = get_text_width("$line &hellip;", $font_file, $font_size);
	while ($width > $text_width && $line =~ s/[.,;:?!\- ]* [^ ]+$//) {
		$width = get_text_width("$line &hellip;", $font_file, $font_size);
	}
	$line = "$line &hellip;";
	return $line;
}

sub write_image {
	my ($img, $file, $jpeg) = @_;
	open(my $fh, '>', $file)
		or croak qq{Cannot open "$file" for writing: $!};
	binmode($fh);
	print $fh $jpeg ? $img->jpeg($config{jpeg_quality}) : $img->png();
	close($fh);
}

sub read_json {
	my ($path) = @_;
	my $contents = read_file($path, 'binmode' => ':utf8');
	return decode_json($contents);
}

sub write_json {
	my ($data, $path) = @_;
	my $contents = encode_json($data);
	write_file($path, { 'binmode' => ':utf8' }, $contents);
}

sub nice_array {
	if (@_ == 1) {
		return $_[0];
	}
	my @entries = map { s/ /&nbsp;/g; $_ } @_;
	return (@entries == 2)
		? join(' and ', @entries)
		: join(', ', @entries[0..$#entries-1]) . ', and ' . $entries[-1];
}

sub remove_duplicates {
	my %seen = ();
	my @result = ();
	foreach my $entry (@_) {
		next if exists $seen{$entry};
		$seen{$entry} = undef;
		push @result, $entry;
	}
	return @result;
}

sub sort_nicely {
	return map { $_->[1] }
		sort { $a->[0] cmp $b->[0] }
		map { [ uc(strip_non_words(unaccent($_))), $_ ] } @_;
}

sub strip_non_words {
	my ($str) = @_;
	$str =~ s/[^A-Za-z0-9]//g;
	return $str;
}

sub strip_article {
	my ($str) = @_;
	$str =~ s/^(?:(?:the|a|an|das|der|het|de|een|le|la|les|un|une|des)\s+|l')//i;
	return $str;
}

sub unaccent {
	my ($in) = @_;
	utf8::encode($in) unless utf8::is_utf8($in);
	return unidecode($in);
}

sub attempt_mkdir {
	my ($path) = @_;
	unless (-e $path) {
		mkdir $path
			or croak qq{Cannot create directory "$path": $!};
	}
}

sub pt2px {
	my ($pt) = @_;
	return $pt * 96 / 72;
}
