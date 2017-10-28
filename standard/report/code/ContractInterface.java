/*
 * An abstract address representation
 */
abstract class Address {
  public int value;
}

/*
 * Contract interface that will be used from outer world
 */
interface ExampleContract {
  /*
   * Returns how many cats account in with given address adopted
   */
  public int getAdoptedCatCount(Address address);
  
  /*
   * Adopt the cat with given id by sender address
   */
  public void adopt(int catId);
}
