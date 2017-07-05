package nio.bytebuffer;

import java.nio.BufferOverflowException;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import org.junit.Assert;
import org.junit.Test;

public class JustPutAndDrainTest {
	@Test
	public void testPutAndDrain() {
		ByteBuffer buffer = ByteBuffer.allocate(4);
		buffer.order(ByteOrder.BIG_ENDIAN);

		buffer.putShort((short) 0xcafe);
		Assert.assertEquals(buffer.capacity() - 2, buffer.remaining());

		Assert.assertEquals((byte) 0x0, buffer.get());
		Assert.assertEquals((byte) 0x0, buffer.get());
		Assert.assertEquals(0, buffer.remaining());
		
		Exception exception = null;
		try {
			Assert.assertEquals((byte) 0x0, buffer.get());
		} catch (BufferUnderflowException ex) {
			exception = ex;
		}
		Assert.assertNotNull(exception);
		
		exception = null;
		try {
			buffer.putShort((short) 0xbabe);
		} catch (BufferOverflowException ex) {
			exception = ex;
		}
		Assert.assertNotNull(exception);
		
		Assert.assertEquals(0, buffer.remaining());
	}
}
