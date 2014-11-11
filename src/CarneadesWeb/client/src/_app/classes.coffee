define [
  'root'
], (cn) ->
  class CarneadesBase
  class CarneadesService extends CarneadesBase
  class CarneadesController extends CarneadesBase

  carneades = cn.carneades
  carneades.Base = CarneadesBase
  carneades.Service = CarneadesService
  carneades.Controller = CarneadesController
