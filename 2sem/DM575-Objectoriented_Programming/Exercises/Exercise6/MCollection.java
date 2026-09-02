package Exercise6;

/**
 * A collection represents a group of objects, known as its elements. 
 * @param <E> the type of elements in this collection.
 */
public interface MCollection<E> extends Iterable<E>{
  
  /**
   * Ensures that this collection contains the specified element
   * @param e
   * @return
   */
  boolean add(E e);
  
  /**
   * Removes all elements in this collection.
   */
  void clear();
  
  /**
   * Checks whether this collection contains o.
   * @param o element whose presence in this collection is to be tested
   * @return true if this collection contains the specified element
   */
  boolean contains(Object o);

  /**
   * Removes a single instance of the specified element from this collection, 
   * if it is present.
   * @param o element to be removed from this collection, if present
   * @return if an element was removed as a result of this call
   */  
  boolean remove(Object o);

  /**
   * Returns the number of elements in this collection.
   * @return the number of elements in this collection.
   */
  int size();

  /**
   * Checks whether this collection is empty.
   * @return true if this collection contains no elements.
   */
  boolean isEmpty();
}