Compressor is standalone compression utility like gzip and bzip2.
It doesn't support any cmdline options but features the same great
compression power as FreeArc itself.

I don't recommend to use it for doing real compression due to it's
lack of CRC checking, file identification and other features common
for Unix compression tools. Consider it as technology demonstration
which may sometimes grow into really useful tool. Compression methods
supported and their parameters are exactly the same as in FreeArc
(so see FreeArc.htm for details).

Usage examples:

Fast binary data compression and decompression:
  compressor tor <example.tar >example.com
  compressor d   <example.com >example.tar

Fast text data compression:
  compressor grzip:m4 <example.tar >example.com

Maximum binary data compression and decompression:
  compressor exe <example.tar | compressor delta | compressor lzma:max:64m >example.com
  compressor d <example.com | compressor d | compressor d >example.tar

Maximum text data compression:
  compressor dict:p:128m <example.tar | compressor lzp:64m:105:d1m:s32:h22 | compressor ppmd:12:192m >example.com
