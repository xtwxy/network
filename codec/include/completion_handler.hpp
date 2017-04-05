#ifndef _CODEC_COMPLETION_HANDLER_H
#define _CODEC_COMPLETION_HANDLER_H

template<typename E, typename M>
class completion_handler {
 public:
  virtual ~completion_handler() = 0;
  virtual void onComplete(E& err, M& msg) = 0;
};
#endif // _CODEC_COMPLETION_HANDLER_H
