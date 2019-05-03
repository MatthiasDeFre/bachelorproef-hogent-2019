using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using System.Text;
using UnityEngine.Profiling;
using UnityEditor;

public class ExperimentProfiler : MonoBehaviour
{
    private float frequency = 1.0f;

    void Start()
    {
        StringBuilder sb = new StringBuilder();
        System.IO.File.WriteAllText(Application.persistentDataPath + "/stats.csv", "Time;Fps;ReservedMemory;AllocatedMemory;GameObjectCount\n");
        StartCoroutine(Stats());
    }

    private IEnumerator Stats()
    {
        for (; ; )
        {
            // FPS
            int lastFrameCount = Time.frameCount;
            float lastTime = Time.realtimeSinceStartup;
            yield return new WaitForSeconds(frequency);
            float timeSpan = Time.realtimeSinceStartup - lastTime;
            int frameCount = Time.frameCount - lastFrameCount;

            // Memory
            long reserved = Profiler.GetTotalReservedMemoryLong();
            long allocated = Profiler.GetTotalAllocatedMemoryLong();

            // Gameobjects
            int gameObjectCount = Resources.FindObjectsOfTypeAll<MonoBehaviour>().Length;

            // Write to csv
            StringBuilder sb = new StringBuilder();
            sb.Append(Time.time).Append(";").Append(Mathf.RoundToInt(frameCount / timeSpan)).Append(';').Append(reserved).Append(';').Append(allocated).Append(";").Append(gameObjectCount).Append('\n');
            System.IO.File.AppendAllText(Application.persistentDataPath + "/stats.csv", sb.ToString());
        }
    }

}
