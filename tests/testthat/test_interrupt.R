context("Interrupt handling")

test_that("R interrupt flag aborts long-running Julia computation", {
  skip_on_cran()
  julia <- julia_setup(installJulia = TRUE)

  # Define a Julia function that:
  # 1. Schedules a task to set R_interrupts_pending after a short delay
  # 2. Runs a long loop with yield points (sleep)
  # 3. Returns how many iterations completed (which is never reached if interrupt works)
  julia_command('
    function _test_interrupt(n, delay_before_interrupt)
        # Schedule a task that sets R_interrupts_pending after a delay,
        # simulating the user pressing Ctrl+C / Stop.
        @async begin
            sleep(delay_before_interrupt)
            if Sys.iswindows()
                ptr = cglobal((:UserBreak, RCall.libR), Cint)
            else
                ptr = cglobal((:R_interrupts_pending, RCall.libR), Cint)
            end
            unsafe_store!(ptr, Cint(1))
        end
        count = 0
        for i in 1:n
            sleep(0.05)  # yield point so timer is triggered
            count += 1
        end
        return count
    end
  ')

  time_in_R <- system.time({
      # Run 200 iterations (would take ~10s), but set interrupt after 0.5s.
      result <- julia_call("_test_interrupt", 200L, 0.5)
  })

  # check 1: this did not take 200 * 0.05 = 10 seconds.
  expect_lt(time_in_R[["elapsed"]], 2.5) # should be around 0.5, but in practice it takes a little longer

  # check 2: return value is NULL
  expect_null(result)

  # check 3: the julia session is still alive
  expect_equal(julia_eval("1 + 1"), 2)

  # check 4: docall's catch block cleared R_interrupts_pending
  flag <- julia_eval("begin
      ptr = @static if Sys.iswindows()
          cglobal((:UserBreak, RCall.libR), Cint)
      else
          cglobal((:R_interrupts_pending, RCall.libR), Cint)
      end
      Int(unsafe_load(ptr))
  end")
  expect_equal(flag, 0L)

  # Unconditionally clear the flag to avoid affecting any subsequent tests.
  julia_command("unsafe_store!(ptr, Cint(0))")
})
