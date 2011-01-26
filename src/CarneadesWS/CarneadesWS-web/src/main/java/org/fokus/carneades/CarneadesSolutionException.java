/*
 * Generel Exception for the Carneades engine
 */

package org.fokus.carneades;

/**
 *
 * @author bbr
 */
public class CarneadesSolutionException extends RuntimeException
{
  public CarneadesSolutionException()
  {
    super();
  }

  public CarneadesSolutionException( String s )
  {
    super( s );
  }
}