package nio.bytebuffer;

import java.nio.ByteBuffer;

import org.junit.Assert;
import org.junit.Test;

public class FlipBufferTest {
	@Test
	public void testPutAndFlip() {
		ByteBuffer buffer = ByteBuffer.allocate(4);
		byte[] expected = new byte[buffer.capacity()];

		for (int i = 0; i < buffer.capacity(); ++i) {
			expected[i] = (byte) i;
			buffer.put((byte) i);
		}
		buffer.flip();
		Assert.assertEquals(buffer.capacity(), buffer.remaining());
		Assert.assertArrayEquals(expected, buffer.array());
		
		for (int i = 0; i < buffer.capacity(); ++i) {
			Assert.assertEquals(expected[i], buffer.get());
		}
		Assert.assertEquals(0, buffer.remaining());
	}
}
