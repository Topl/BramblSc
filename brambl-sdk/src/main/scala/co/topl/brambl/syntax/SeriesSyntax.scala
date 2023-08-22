package co.topl.brambl.syntax

import co.topl.brambl.models.SeriesId
import co.topl.brambl.models.box.Value.Series
import scala.language.implicitConversions

trait SeriesSyntax {

  implicit def seriesAsSeriesSyntaxOps(series: Series): SeriesAsSeriesSyntaxOps =
    new SeriesAsSeriesSyntaxOps(series)
}

class SeriesAsSeriesSyntaxOps(val series: Series) extends AnyVal {

  /**
   * The ID of this series.  If an ID was pre-computed and saved in the series, it is restored.
   * Otherwise, a new ID is computed (but not saved in the Series).
   */
  def id: SeriesId = series.seriesId.getOrElse(computeId)

  /**
   * Computes what the ID _should_ be for this Series.
   */
  def computeId: SeriesId = series.seriesPolicy.computeId

  /**
   * Compute a new ID and return a copy of this Series with the new ID embedded.
   * Any previous value will be overwritten in the new copy.
   */
  def embedId: Series =
    series.copy(seriesId = Some(computeId))

  /**
   * Returns true if this Series contains a valid embedded ID.
   */
  def containsValidId: Boolean =
    series.seriesId.contains(computeId)
}
