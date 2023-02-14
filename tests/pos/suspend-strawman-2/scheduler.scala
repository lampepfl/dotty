package concurrent

/** A hypothetical task scheduler trait */
trait Scheduler:
  def schedule(task: Runnable): Unit = ???

object Scheduler extends Scheduler:
  given fromAsyncConfig(using ac: Async.Config): Scheduler = ac.scheduler
end Scheduler

