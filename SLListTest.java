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
    public void testSLListReverse_1() {
        SLList test1 = SLList.of(1);
        SLList test2 = SLList.of(1);
        assertEquals(test1, test2);

        test1.reverse();
        assertEquals(1, test1.size());
        assertEquals(1, test1.get(0));
    }

    @Test
    public void testSLListReverse_2() {
        SLList test1 = new SLList();
        SLList test2 = new SLList();
        assertEquals(test1, test2);

        test1.reverse();
        assertEquals(0, test1.size());
        assertEquals(42, test1.get(0)); //an empty list is represented by just a sentinel
        // sentinel node always exists (even when our list is empty)
    }

    @Test
    public void testSLListReverse_3() {
        SLList test1 = SLList.of(1, 2, 3, 4); //I expect [1,2,3,4] to become [4,3,2,1]
        test1.reverse();
        assertEquals(4, test1.size());
        assertEquals(1, test1.get(0));
        assertEquals(2, test1.get(1));
        assertEquals(3, test1.get(2));
        assertEquals(4, test1.get(3));

    }

}
