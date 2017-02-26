public class A {
    public void m1() {}

    public void m2() {
        m1();
        (new Ext()).mext();
        1 + true;
    }
}

public class Ext {
    public int mext() {}
}