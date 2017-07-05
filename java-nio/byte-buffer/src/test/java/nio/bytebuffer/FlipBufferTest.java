package nio.bytebuffer;

import java.nio.ByteBuffer;

import org.junit.Assert;
import org.junit.Test;

public class FlipBufferTest {
	@Test
	public void testPutFullAndFlip() {
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

	@Test
	public void testFlipEmptyBuffer() {
		ByteBuffer buffer = ByteBuffer.allocate(4);

		Assert.assertEquals(buffer.capacity(), buffer.remaining());
		buffer.flip();

		Assert.assertEquals(0, buffer.remaining());
	}

	@Test
	public void testPutOneAndFlip() {
		ByteBuffer buffer = ByteBuffer.allocate(4);

		Assert.assertEquals(buffer.capacity(), buffer.remaining());
		buffer.put((byte) 0xfe);
		buffer.flip();

		Assert.assertEquals(1, buffer.remaining());
	}
}
