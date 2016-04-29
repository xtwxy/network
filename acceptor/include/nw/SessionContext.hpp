class SessionContext {
public:
    void onMessage(boost::any msg) {
    }
    void write(boost::any msg) {
        prev.write(codecFactory.getEncoder().encode(this, msg));
    }
    void close() {
    }
private:
    Decoder getDecoder() {
        return codecFactory.getDecoder();
    }
    Encoder getDecoder() {
        return codecFactory.getEncoder();
    }
    SessionContext next;
    SessionContext prev;
    CodecFactory codecFactory;
};


