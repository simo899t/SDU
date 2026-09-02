package Exercise6;

import java.util.Iterator;

/**
 * An iterator for ordered collections.
 * This interface is a simplified version of {@code java.utils.ListIterator}.
 * 
 * @param <E> the type of elements in this list.
 * @see <a href=
 *      "https://docs.oracle.com/javase/8/docs/api/java/util/List.html">java.utils.ListIterator</a>
 */
public interface MListIterator<E> extends Iterator<E> {
  /**
   * Returns the index of the element that would be returned by a
   * subsequent call to {@link #next}. (Returns list size if the list
   * iterator is at the end of the list.)
   *
   * @return the index of the element that would be returned by a
   *         subsequent call to {@code next}, or list size if the list
   *         iterator is at the end of the list
   */
  int nextIndex();
}
