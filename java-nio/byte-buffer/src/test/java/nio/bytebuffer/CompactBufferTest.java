package nio.bytebuffer;

import java.nio.ByteBuffer;

import org.junit.Assert;
import org.junit.Test;

public class CompactBufferTest {
	@Test
	public void testCompactEmptyBuffer() {
		ByteBuffer buffer = ByteBuffer.allocate(4);
		Assert.assertEquals(buffer.capacity(), buffer.remaining());
		buffer.compact();

		Assert.assertEquals(0, buffer.remaining());
	}

	@Test
	public void testPutOneAndCompact() {
		ByteBuffer buffer = ByteBuffer.allocate(4);
		Assert.assertEquals(buffer.capacity(), buffer.remaining());

		buffer.put((byte) 0xff);
		Assert.assertEquals(buffer.capacity() - 1, buffer.remaining());
		buffer.flip();
		Assert.assertEquals(1, buffer.remaining());
		buffer.compact();
		Assert.assertEquals(buffer.capacity() - 1, buffer.remaining());
	}

	@Test
	public void testPutFullAndCompact() {
		ByteBuffer buffer = ByteBuffer.allocate(4);
		Assert.assertEquals(buffer.capacity(), buffer.remaining());
		for (int i = 0; i < buffer.capacity(); ++i) {
			Assert.assertEquals(buffer.capacity() - i, buffer.remaining());
			buffer.put((byte) (0xff - i));
			Assert.assertEquals(buffer.capacity() - i - 1, buffer.remaining());
		}
		buffer.compact();

		Assert.assertEquals(4, buffer.remaining());

		Assert.assertEquals((byte) 0xff, buffer.get());
		Assert.assertEquals(3, buffer.remaining());

		Assert.assertEquals((byte) 0xfe, buffer.get());
		Assert.assertEquals(2, buffer.remaining());

		Assert.assertEquals((byte) 0xfd, buffer.get());
		Assert.assertEquals(1, buffer.remaining());

		Assert.assertEquals((byte) 0xfc, buffer.get());
		Assert.assertEquals(0, buffer.remaining());
	}
}
