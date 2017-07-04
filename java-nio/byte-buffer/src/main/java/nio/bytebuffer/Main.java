package nio.bytebuffer;

import static java.lang.System.out;

import java.nio.ByteBuffer;

public class Main {
	public static void main(String[] args) {
		ByteBuffer buffer = ByteBuffer.allocate(8);
		
		out.println(String.format("buffer.capacity() = %s", buffer.capacity()));
		out.println(String.format("buffer.remaining() = %s", buffer.remaining()));
	
		for(int i = 0; i < 512; ++i) {
			out.println(String.format("buffer[%s] = %s, %s bytes remaining, capacity = %s.", 
					i, buffer.get(), buffer.remaining(), buffer.capacity()));
		}
	}
}
