//Rafał Kruszyna
public class test {
    public static void main(String[] args) throws FullException, EmptyException {
        MyQueue<String> queue = new CyclicArrayQueue<>(3);
        System.out.println(queue.isEmpty());
        System.out.println(queue.isFull());
        queue.enqueue("Italy2006");
        queue.enqueue("Malaysia2008");
        queue.enqueue("Bahrain2008");
        System.out.println(queue.isEmpty());
        System.out.println(queue.isFull());
        System.out.println(queue.first());
        queue.dequeue();
        queue.enqueue("Monaco2008");
        System.out.println(queue.first());
        queue.dequeue();
        queue.enqueue("Canada2008-WIN");
        System.out.println(queue.first());
        queue.dequeue();
        System.out.println(queue.first());
        queue.dequeue();
        System.out.println(queue.first());
        queue.dequeue();
        System.out.println(queue.isEmpty());
        System.out.println(queue.isFull());


    }

}
