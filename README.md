# ffmpegcrop

First install ghc 9.8 and cabal-install. I recommend using [ghcup](https://www.haskell.org/ghcup/install/).

> apt install libsdl2-dev ffmpeg
> git clone https://github.com/aavogt/ffmpegcrop.git && cd ffmpegcrop && cabal install

        The ffmpegcrop program
        
        ffmpegcrop [OPTIONS] INFILE
          ffmpegcrop displays the first video frame with a gray rectangle overlay, left
          click to move the closest corner to that point and press Enter to transcode
          that region to a new file
        
        Common flags:
          -f     --format=ITEM   output file extension (webm) by default
          -u     --uniq=ITEM     output file suffix can contain %d for the duration
                                 of the crop in seconds, %w for the width, %h for the
                                 height, %x for the x position, %y for the y position
                                 and %% for a literal %. Default: _cropped_%w_%h
          -s     --start=INT     start time in seconds
          -d     --duration=INT  duration in seconds
          -w     --width=INT     initial width
          -h     --height=INT    initial height
          -x=INT                 initial x position
          -y=INT                 initial y position
          -?     --help          Display help message
          -V     --version       Print version information

# TODO

- [ ] set -s and -d with a montage and scroll wheel
- [ ] .desktop file for `alias ffmpegcropsel='ffmpegcrop $(zenity --file-selection --file-filter="*.mp4 *.mkv *.webm *.avi *.mov *.flv *.wmv *.mpg *.mpeg *.m4v *.ts *.vob *.ogv *.ogg *.3gp *.3g2 *.asf *.m2ts *.mts *.m2v *.m4v"'`
