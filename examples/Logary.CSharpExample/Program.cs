﻿using System;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using NodaTime;
using Logary.Configuration;
using Logary.CSharp;
using Logary.Metrics;
using Logary.Targets;
using Uri = System.Uri;

/*
 * 
    
//   .Target<Logary.Targets.RabbitMQ.Builder>(
//       "rabbitmq",
//       conf => conf.Target.EnableTls("./cert/path.pfx", "TopSecret12345").Done()
//   )
// currently Windows only:
/*
    .Target<DB.Builder>("db",
        conf => conf.Target
            .ConnectionFactory(() => new SQLiteConnection())
            .DefaultSchema()
            .MigrateUp(
                conn => new SqliteProcessor(conn,
                    new SqliteGenerator(),
                    new ConsoleAnnouncer(),
                    new MigrationOptions(false, "", 60),
                                            new SqliteDbFactory())))
*/

namespace Logary.CSharpExample
{
    public static class Program
    {
        public static async Task SampleUsage(Logger logger)
        {
            // await placing the Hello World event in the buffer
            await logger.LogEvent(LogLevel.Debug, "Hello world. Important? {important}", new
            {
                important = "yes"
            }, backpressure: true);

            // await logging the fatal event and getting an ack back from each of the configured
            // targets
            await logger.LogEvent(LogLevel.Fatal, "Fatal application error on finaliser thread.", flush: true);

            await logger.LogEvent(LogLevel.Verbose, "We need to log this with backpressure.", new
            {
                tags = new[] { "tag1", "tag2" }
            }, backpressure: true);

            // alternatively, you can use the ack-explicit functions together with the
            // data object model that's MessageModule.
            var message = MessageModule.Event(LogLevel.Warn, "Here be dragons!");
            var ack = await logger.LogWithAck(message);
            await ack;

            var val = logger.Time(() =>
                    {
                        for (int i = 0; i < 100; i++)
                            System.Threading.Thread.Sleep(1);

                        return 32;
                    }, "sample.config.computeAnswerToEverything")
                ();

            await logger.LogEventFormat(LogLevel.Warn,
                "{horses} is the answer to the universe and everything",
                val);

            await logger.Time(
                    () => logger.LogEvent(LogLevel.Debug, "I wonder how long this takes"))
                ();

            try
            {
                throw new ApplicationException("thing went haywire");
            }
            catch (Exception e)
            {
                await logger.LogEventFormat(LogLevel.Fatal, "Unhandled {exception}!", e);
            }
        }

        public static int Main(string[] args)
        {
            var mre = new ManualResetEventSlim(false);
            System.Console.CancelKeyPress += (sender, arg) => mre.Set();

            using (var logary = LogaryFactory.New("Logary.CSharpExample",
                with => with.InternalLoggingLevel(LogLevel.Debug)
                        .Metrics(m =>
                            m.AddMetric(Duration.FromSeconds(3L), "appMetrics", WinPerfCounters.appMetrics)
                             .AddMetric(Duration.FromSeconds(3L), "systemMetrics", WinPerfCounters.systemMetrics))
                        .Target<TextWriter.Builder>(
                          "console1",
                          conf =>
                            conf.Target.WriteTo(System.Console.Out, System.Console.Error)
                                .MinLevel(LogLevel.Verbose)
                                .AcceptIf(line => true)
                                .SourceMatching(new Regex(".*"))
                            )
                            .Target<Graphite.Builder>(
                                "graphite",
                                conf => conf.Target.ConnectTo("127.0.0.1", 2131)
                            )
                            .Target<Debugger.Builder>("debugger")
                            .Target<Logstash.Builder>(
                                "logstash",
                                conf => conf.Target
                                    .PublishTo("logstash.service")
                                    .LogMetrics()
                                    .Done()
                            )
                            .Target<ElasticSearch.Builder>(
                                "es",
                                conf => conf.Target
                                    .PublishTo("elasticsearch.service")
                                    .Type("logs") // index-name
                                    .Done()
                            )
                            //.Target<File.Builder>(
                            //    "file",
                            //    conf => conf
                            //)
                            .Target<InfluxDb.Builder>("influx",
                                                      conf => conf.Target.DB("http://influxdb.service:8086").Done())
                ).Result)
            {
                //var logger = logary.GetLogger("main");
                //SampleUsage(logger).Wait();
                mre.Wait();
            }

            return 0;
        }
    }
}

