package nio.bytebuffer;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import org.junit.Assert;
import org.junit.Test;


public class PutFlipGetCompatTest {
	@Test
	public void testPutFlipGetCompact() {
		ByteBuffer buffer = ByteBuffer.allocate(5);
		buffer.order(ByteOrder.BIG_ENDIAN);

		buffer.putShort((short) 0xcafe);
		Assert.assertEquals(buffer.capacity() - 2, buffer.remaining());

		buffer.flip();
		Assert.assertEquals(2, buffer.remaining());
		
		Assert.assertEquals((byte)0xca, buffer.get());
		
		buffer.compact();
		Assert.assertEquals(buffer.capacity() - 1, buffer.remaining());
		buffer.putShort((short) 0xbabe);
		Assert.assertEquals(buffer.capacity() - 3, buffer.remaining());
		buffer.flip();
		Assert.assertEquals(3, buffer.remaining());
		
		Assert.assertEquals((byte)0xfe, buffer.get());
		Assert.assertEquals((byte)0xba, buffer.get());
		Assert.assertEquals((byte)0xbe, buffer.get());
		Assert.assertEquals(0, buffer.remaining());
		
		buffer.compact();
		Assert.assertEquals(buffer.capacity(), buffer.remaining());
	}
}
