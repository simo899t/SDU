package Exercise6;

/**
 * An ordered collection.
 * This interface is a simplified version of {@code java.utils.List}.
 * 
 * @param <E> the type of elements in this list.
 * @see <a href=
 *      "https://docs.oracle.com/javase/8/docs/api/java/util/List.html">java.utils.List</a>
 */
public interface MList<E> extends MCollection<E> {
  /**
   * Appends the specified element to the end of this list.
   * 
   * @param e element to be appended to this list
   * @return {@code true} (as specified by {@link MCollection#add})
   */
  @Override
  boolean add(E e);

  /**
   * Returns the element at the specified position in this list.
   * 
   * @param index index of the element to return
   * @return the element at the specified position in this list
   * @throws IndexOutOfBoundsException - if the index is out of range [0,size())
   */
  E get(int index);

  /**
   * Returns a list iterator over elements of type {@code E}.
   *
   * @return a list iterator.
   */
  @Override
  MListIterator<E> iterator();

}
