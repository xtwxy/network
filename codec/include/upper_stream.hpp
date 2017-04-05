#ifndef _CODEC_UPPER_STREAM_H
#define _CODEC_UPPER_STREAM_H

template<typename M>
class upper_stream {
 public:
  virtual ~upper_stream() = 0;
  virtual void write(M& msg) = 0;
  virtual void notifyTimeout() = 0;
  virtual void notifyClose() = 0;
};
#endif // _CODEC_UPPER_STREAM_H
