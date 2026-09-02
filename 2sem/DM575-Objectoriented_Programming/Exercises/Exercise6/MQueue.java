package Exercise6;

/**
 * A collection where elements are added and retrieved in a FIFO
 * (first-in-first-out) order..
 * This interface is a simplified version of {@code java.utils.Queue}.
 * 
 * @param <E> the type of elements in this queue.
 * @see <a href=
 *      "https://docs.oracle.com/javase/8/docs/api/java/util/Queue.html">java.utils.Queue</a>
 */
public interface MQueue<E> extends MCollection<E> {
  /**
   * Appends the specified element to the end of this queue.
   * 
   * @param e element to be appended to this queue
   * @return {@code true} (as specified by {@link MCollection#add})
   */
  @Override
  boolean add(E e);

  /**
   * Retrieves, but does not remove, the head of this queue.
   * 
   * @return the head of this queue
   * @throws NoSuchElementException if this queue is empty
   */
  E element();

  /**
   * Retrieves and removes the head of this queue.
   * 
   * @return the head of this queue
   * @throws NoSuchElementException if this queue is empty
   */
  E remove();

}
