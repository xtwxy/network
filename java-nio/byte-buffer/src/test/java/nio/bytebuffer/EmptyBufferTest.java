package nio.bytebuffer;

import java.nio.BufferOverflowException;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;

import org.junit.Assert;
import org.junit.Test;

public class EmptyBufferTest {
	@Test
	public void testGet() {
		ByteBuffer buffer = ByteBuffer.allocate(4);

		Assert.assertEquals(buffer.remaining(), buffer.capacity());
		
		Exception exception = null;

		try {
			for (int i = 0; i < buffer.capacity() + 1; ++i) {
				buffer.get();
				Assert.assertEquals(buffer.remaining() + i + 1, buffer.capacity());
			}
		} catch (BufferUnderflowException ex) {
			exception = ex;
		}
		Assert.assertNotNull(exception);
	}

	@Test
	public void testPut() {
		ByteBuffer buffer = ByteBuffer.allocate(4);

		Assert.assertEquals(buffer.remaining(), buffer.capacity());
		
		Exception exception = null;

		try {
			for (int i = 0; i < buffer.capacity() + 1; ++i) {
				buffer.put((byte) i);
				Assert.assertEquals(buffer.remaining() + i + 1, buffer.capacity());
			}
		} catch (BufferOverflowException ex) {
			exception = ex;
		}
		Assert.assertNotNull(exception);
	}
}
