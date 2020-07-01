import org.junit.Test;
import static org.junit.Assert.*;

public class SLListTest {

    @Test
    public void testSLListAdd() {
        SLList test1 = SLList.of(1, 3, 5);
        SLList test2 = new SLList();

        test1.add(1, 2);
        test1.add(3, 4);
        assertEquals(5, test1.size());
        assertEquals(3, test1.get(2));
        assertEquals(4, test1.get(3));

        test2.add(1, 1);
        assertEquals(1, test2.get(0));
        assertEquals(1, test2.size());

        test2.add(10, 10);
        assertEquals(10, test2.get(1));
        test1.add(0, 0);
        assertEquals(SLList.of(0, 1, 2, 3, 4, 5), test1);
    }

    @Test
    public void testSLListReverse() {
        //general case for lists of size >= 2
        SLList A_1 = SLList.of(1,2,3);
        SLList A_2 = SLList.of(3,2,1);
        assertEquals(A_1, A_2);

        SLList B_1 = SLList.of(11,22,33,44);
        SLList B_2 = SLList.of(44,33,22,11);
        assertEquals(B_1, B_2);

        //base case: function reverses a list of size 1 by completing without erroring
        SLList C_1 = SLList.of(1);
        SLList C_2 = SLList.of(1);
        assertEquals(C_1, C_2);

        //base case: function reverses a list of size 0 by completing without erroring
        SLList D_1 = SLList.of();
        SLList D_2 = SLList.of();
        assertEquals(D_1, D_2);
    }

}
