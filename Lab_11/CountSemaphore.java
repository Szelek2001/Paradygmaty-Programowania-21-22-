//Zadanie 2b
import java.util.concurrent.Semaphore;

class IntCellSem {
    private int n = 0;
    private static Semaphore semaphore = new Semaphore(1);


    public int getN() {return n;}
    public void setN(int n) {this.n = n;}
    public void acquireSem() {
        try {
            semaphore.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
    public void releaseSem() {semaphore.release();}
}

class CountSemaphor extends Thread {
    private static IntCellSem n = new IntCellSem();

    @Override public void run() {
        int temp;

        for (int i = 0; i < 200000; i++) {
            n.acquireSem();
            temp = n.getN();
            n.setN(temp + 1);
            n.releaseSem();
        }
    }

    public static void main(String[] args) {
        CountSemaphor p = new CountSemaphor();
        CountSemaphor q = new CountSemaphor();
        p.start();
        q.start();


        try { p.join(); q.join(); }
        catch (InterruptedException e) { }
        System.out.println("The value of n is " + n.getN());
    }
}