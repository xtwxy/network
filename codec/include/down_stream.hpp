#ifndef _CODEC_DOWN_STREAM_H
#define _CODEC_DOWN_STREAM_H

template<typename M>
class down_stream {
 public:
  virtual ~down_stream() = 0;
  virtual void write(M& msg) = 0;
  virtual void notifyTimeout() = 0;
  virtual void notifyClose() = 0;
};
#endif // _CODEC_DOWN_STREAM_H
