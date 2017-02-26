public class A {
    public void m1() {}

    public void m2() {
        m1();
        //m3();
        this.m1();
    }
}