//Rafał Kruszyna
import java.util.ArrayList;

public class CyclicArrayQueue<E> implements MyQueue<E> {
    private int f = 0;
    private int r = 0;
    private ArrayList<E> queue;

    public CyclicArrayQueue(int n) {
        queue = new ArrayList<>(n + 1);
        f = 0;
        r = 0;
        for (int i = 0; i < n+1; i++) {
            queue.add(null);
        }

    }

    @Override
    public void enqueue(E x) throws FullException {
        if (isFull())
            throw new FullException("Kolejka jest pełna");
        else {
            queue.set(r, x);
            r = r >= queue.size() - 1 ? 0 : r + 1;
        }
    }

    @Override
    public void dequeue() {
        if (!isEmpty())
            f = f >= queue.size() - 1 ? 0 : f + 1;

    }

    @Override
    public E first() throws EmptyException {
        if(isEmpty()) throw new EmptyException("Kolejka jest pusta");
        else return  queue.get(f);
    }

    @Override
    public boolean isEmpty() {
        return f == r;
    }

    @Override
    public boolean isFull() {
        return (f - r == 1 || r - f == queue.size() - 1);
    }
}
