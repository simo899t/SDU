package Exercise6;

/**
 * A collection that contains no duplicate elements.
 * This interface is a simplified version of {@code java.utils.Set}.
 * 
 * @param <E> the type of elements in this set.
 * @see <a href=
 *      "https://docs.oracle.com/javase/8/docs/api/java/util/Set.html">java.utils.Set</a>
 */
public interface MSet<E> extends MCollection<E> {
  /**
   * Adds the specified element to this set if it is not already present.
   * 
   * @param e element to be added to this set
   * @return {@code true} if this set did not already contain the specified
   *         element
   */
  @Override
  boolean add(E e);
}
