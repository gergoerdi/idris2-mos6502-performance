module Kastely.Text

%default total

public export
toChar : Bits8 -> Char
toChar b = case b of
  0x1b => 'á'
  0x1c => 'é'
  0x1d => 'í'
  0x1e => 'ó'
  0x1f => 'ü'
  0x23 => 'ő'
  0x24 => 'ö'
  0x25 => 'ú'
  0x26 => 'ű'
  0x21 => '!'
  0x22 => '"'
  0x27 => '\''
  0x2c => ','
  0x2d => '-'
  0x2e => '.'
  0x2f => ' '
  0x3a => ':'
  0x3b => ';'
  0x20 => ' '
  _ => if b <= 0x1a then cast $ b + 0x60
    else if 0x30 <= b && b <= 0x39 then cast b
    else if 0x40 < b && b < 0x80 then cast $ b - 0x40
    else '_'
