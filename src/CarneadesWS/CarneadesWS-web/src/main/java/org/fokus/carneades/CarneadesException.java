/*
 * Generel Exception for the Carneades engine
 */

package org.fokus.carneades;

/**
 *
 * @author bbr
 */
public class CarneadesException extends RuntimeException
{
  public CarneadesException()
  {
    super();
  }

  public CarneadesException( String s )
  {
    super( s );
  }
}