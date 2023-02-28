Imports EEPHASE

Module Utils

    Public Const LEAKS_ID As String = "leaks"
    Public Const CMD2M3 As Double = 86.4
    Public Const RALCO_INTER_VOL As Double = 757000
    Public Const LMAULE_INTER_VOL As Double = 452000 '1000m3
    Public Const LMAULE_EXTRAO_VOL As Double = 129000 '1000m3
    Public Const LAJA_MAX_EXT_GEN As Double = 1200000 '1000m3
    Public Const FCF_MIN_VALUE As Double = 0.0 '1000m3
    Public Const FREE_FLOW As Double = 9999 'usually in cumec
    Public Const FREE_RALCO As Double = 12824.1 '1000m3

    Private USE_EXTRAO_AS_LOW As Boolean = False

    Function IsFiltration(c As Constraint) As Boolean
        Return c.Name.ToLower().Contains(LEAKS_ID)
    End Function

    Function isRiegoVariable(v As Variable) As Boolean
        Return (v.Name.Equals("RiegoLoad_CMelado", StringComparison.OrdinalIgnoreCase) OrElse
            v.Name.Equals("RiegoLoad_CMNA", StringComparison.OrdinalIgnoreCase) OrElse
            v.Name.Equals("RiegoLoad_CMNB", StringComparison.OrdinalIgnoreCase) OrElse
            v.Name.Equals("RiegoLoad_Maitenes", StringComparison.OrdinalIgnoreCase) OrElse
            v.Name.Equals("RiegoLoad_MauleSur", StringComparison.OrdinalIgnoreCase) OrElse
            v.Name.Equals("RiegoLoad_MolinosOtros", StringComparison.OrdinalIgnoreCase) OrElse
            v.Name.Equals("RiegoLoad_Sur123SCDZ", StringComparison.OrdinalIgnoreCase)
            )
    End Function

    Function IsHoyaIntermedia(v As Variable) As Boolean
        Return (v.Name.Equals("COLBUN", StringComparison.OrdinalIgnoreCase) OrElse
            v.Name.Equals("B_M_Isla", StringComparison.OrdinalIgnoreCase) OrElse
            v.Name.Equals("B_Maule", StringComparison.OrdinalIgnoreCase) OrElse
            v.Name.Equals("CMNTronco", StringComparison.OrdinalIgnoreCase) OrElse
            v.Name.Equals("RioMelado", StringComparison.OrdinalIgnoreCase)) OrElse
            v.Name.Equals("LAMINA", StringComparison.OrdinalIgnoreCase)
    End Function

    Function GetStorage(c As Constraint) As Storage

        If c.Storages.Count > 0 Then
            Return c.Storages.First()
        Else
            For Each s As Storage In StoragesIN
                If (c.Name.ToLower().Contains(s.Name.ToLower())) Then
                    Return s
                End If
            Next
        End If
        Return Nothing

    End Function

    Function GetStoraName(c As Constraint) As String
        Dim storage As Storage
        storage = GetStorage(c)
        If (IsNothing(storage)) Then
            Return ""
        Else
            Return storage.Name
        End If
    End Function

    ''' <summary>
    ''' Generic filtration function
    ''' </summary>
    ''' <param name="strName">Contraint name</param>
    ''' <param name="Vol">Volume in 1000m3</param>
    ''' <returns>Filtration value for giving volume</returns>
    Function GetFiltration(strName As String, Vol As Double) As Double

        Vol /= CMD2M3 'Convert to CMD

        If strName.ToLower().Contains("eltoro") Then
            Return GetFiltrationElToro(Vol)
        ElseIf strName.ToLower().Contains("colbun") Then
            Return GetFiltrationColbun(Vol)
        ElseIf strName.ToLower().Contains("cipreses") Then
            Return GetFiltrationCipreses(Vol)
        Else
            Return 0.0
        End If

    End Function

    ''' <summary>
    ''' Generic filtration linear coefficient function
    ''' </summary>
    ''' <param name="strName">Contraint name</param>
    ''' <param name="Vol">Volume in 1000m3</param>
    ''' <returns>Linear coefficient value for giving volume</returns>
    Function GetFiltrationLinearCoefficient(strName As String, Vol As Double) As Double
        If strName.ToLower().Contains("eltoro") Then
            Return GetFiltrationLinearCoefficientElToro(Vol)
        ElseIf strName.ToLower().Contains("colbun") Then
            Return GetFiltrationLinearCoefficientColbun(Vol)
        ElseIf strName.ToLower().Contains("cipreses") Then
            Return GetFiltrationLinearCoefficientCipreses(Vol)
        Else
            Return 0.0
        End If
    End Function

    ''' <summary>
    ''' Generic filtration linear function intercept (RHS)
    ''' </summary>
    ''' <param name="strName">Contraint name</param>
    ''' <param name="Vol">Volume in 1000m3</param>
    ''' <returns>Filtration linear function intercept (RHS)</returns>
    Function GetFiltrationLinearRHS(strName As String, Vol As Double) As Double
        If strName.ToLower().Contains("eltoro") Then
            Return GetFiltrationLinearRHSElToro(Vol)
        ElseIf strName.ToLower().Contains("colbun") Then
            Return GetFiltrationLinearRHSColbun(Vol)
        ElseIf strName.ToLower().Contains("cipreses") Then
            Return GetFiltrationLinearRHSCipreses(Vol)
        Else
            Return 0.0
        End If
    End Function

    Function GetFiltrationCipreses(Vol As Double) As Double
        Dim dCota As Double
        Dim dFilt As Double
        Dim a0 As Double = 134744.88984
        Dim a1 As Double = -211.91025423
        Dim a2 As Double = 0.0833132678

        dCota = (-a1 + Math.Sqrt(a1 * a1 - 4 * a2 * (a0 - Vol * 0.864))) / (2 * a2)

        If (dCota <= 1307) Then
            dFilt = 0.158 * dCota - 192.212
        Else
            dFilt = 0.531 * dCota - 679.985
        End If
        Return dFilt
    End Function

    Function GetFiltrationLinearCoefficientCipreses(vol As Double) As Double
        If vol < 236 Then
            Return 0.0
        ElseIf 236 <= vol AndAlso vol < 4716 Then
            Return 0.0
        ElseIf 4716 <= vol AndAlso vol < 13000 Then
            Return -0.001337199
        ElseIf 13000 <= vol AndAlso vol < 98000 Then
            Return -0.000037851
        Else
            Return -0.000074571
        End If
    End Function

    Function GetFiltrationLinearRHSCipreses(vol As Double) As Double
        If vol < 236 Then
            Return 0.0
        ElseIf 236 <= vol AndAlso vol < 4716 Then
            Return 10.25
        ElseIf 4716 <= vol AndAlso vol < 13000 Then
            Return -6.30623048
        ElseIf 13000 <= vol AndAlso vol < 98000 Then
            Return 10.58481
        Else
            Return 6.98653
        End If
    End Function

    Function GetFiltrationElToro(Vol As Double) As Double

        Dim dFilt As Double

        Dim a6 As Double = 1.093302E-26
        Dim a5 As Double = -9.849929E-22
        Dim a4 As Double = 2.720852E-17
        Dim a3 As Double = 0.000000000000124898
        Dim a2 As Double = -0.00000001934762
        Dim a1 As Double = 0.0007673328
        Dim a0 As Double = 14.09851

        Dim b6 As Double = -5.647914E-28
        Dim b5 As Double = 1.755021E-22
        Dim b4 As Double = -2.227358E-17
        Dim b3 As Double = 0.000000000001488955
        Dim b2 As Double = -0.00000005150526
        Dim b1 As Double = 0.001326569
        Dim b0 As Double = 9.688092

        Dim c6 As Double = 4.954184E-27
        Dim c5 As Double = -7.770996E-22
        Dim c4 As Double = 3.245041E-17
        Dim c3 As Double = 0.000000E+00
        Dim c2 As Double = 0.000000E+00
        Dim c1 As Double = 0.000000E+00
        Dim c0 As Double = -0.5886818

        Dim R1 As Double = 20818.61302
        Dim R2 As Double = 62724.15443

        If Vol < R1 Then
            dFilt = a0 + a1 * Vol + a2 * Math.Pow(Vol, 2) + a3 * Math.Pow(Vol, 3) + a4 * Math.Pow(Vol, 4) + a5 * Math.Pow(Vol, 5) + a6 * Math.Pow(Vol, 6)
        ElseIf R1 <= Vol AndAlso Vol <= R2 Then
            dFilt = b0 + b1 * Vol + b2 * Math.Pow(Vol, 2) + b3 * Math.Pow(Vol, 3) + b4 * Math.Pow(Vol, 4) + b5 * Math.Pow(Vol, 5) + b6 * Math.Pow(Vol, 6)
        Else
            dFilt = c0 + c1 * Vol + c2 * Math.Pow(Vol, 2) + c3 * Math.Pow(Vol, 3) + c4 * Math.Pow(Vol, 4) + c5 * Math.Pow(Vol, 5) + c6 * Math.Pow(Vol, 6)
        End If

        Return dFilt
    End Function

    Function GetFiltrationLinearCoefficientElToro(vol As Double) As Double
        If vol < 28000 Then
            Return -0.000058532
        ElseIf 28000 <= vol AndAlso vol < 2700000 Then
            Return -0.00000552
        Else
            Return -0.000007149
        End If
    End Function

    Function GetFiltrationLinearRHSElToro(vol As Double) As Double
        If vol < 28000 Then
            Return 0.0
        ElseIf 28000 <= vol AndAlso vol < 2700000 Then
            Return 14.843218
        Else
            Return 10.44411
        End If
    End Function

    Function GetFiltrationColbun(vol As Double) As Double

        Dim dFilt As Double

        Dim a0 As Double = -16.92563
        Dim a1 As Double = 0.00398733
        Dim a2 As Double = -0.0000003760689
        Dim a3 As Double = 0.00000000002608552
        Dim a4 As Double = -0.000000000000001144485
        Dim a5 As Double = 2.822735E-20
        Dim a6 As Double = -2.9421E-25

        Dim b0 As Double = -25.55583
        Dim b1 As Double = 0.005251315
        Dim b2 As Double = -0.0000004281374
        Dim b3 As Double = 0.00000000002560029
        Dim b4 As Double = -0.0000000000000009703281
        Dim b5 As Double = 2.092268E-20
        Dim b6 As Double = -1.956679E-25

        Dim R1 As Double = 7645.679032
        Dim R2 As Double = 11336.34746

        If vol < R1 Then
            dFilt = 0
        ElseIf R1 <= vol AndAlso vol <= R2 Then
            dFilt = a0 + a1 * vol + a2 * Math.Pow(vol, 2) + a3 * Math.Pow(vol, 3) + a4 * Math.Pow(vol, 4) + a5 * Math.Pow(vol, 5) + a6 * Math.Pow(vol, 6)
        Else
            dFilt = b0 + b1 * vol + b2 * Math.Pow(vol, 2) + b3 * Math.Pow(vol, 3) + b4 * Math.Pow(vol, 4) + b5 * Math.Pow(vol, 5) + b6 * Math.Pow(vol, 6)
        End If

        Return dFilt
    End Function

    Function GetFiltrationLinearCoefficientColbun(vol As Double) As Double
        If vol < 660000 Then
            Return 0.0
        Else
            Return -0.0000118
        End If
    End Function

    Function GetFiltrationLinearRHSColbun(vol As Double) As Double
        If vol < 660000 Then
            Return 0.0
        Else
            Return -7.7963
        End If
    End Function

    Friend Function IsLajaInitDate(dateCur As Date) As Boolean
        Return (dateCur.Month = 1 AndAlso dateCur.Day = 8 AndAlso dateCur.Hour = 0)
    End Function

    Friend Function IsMauleInitDate(dateCur As Date) As Boolean
        Return (dateCur.Month = 1 AndAlso dateCur.Day = 1 AndAlso dateCur.Hour = 0)
    End Function

    Sub FillVolume(initVol As Double, interVol As Double, interPeriod As Integer, ByRef Vol() As Double)
        For i As Integer = 1 To Vol.Length - 1
            If i < interPeriod Then
                Vol(i) = initVol / CMD2M3 'converted to CMD
            Else
                Vol(i) = interVol / CMD2M3 'converted to CMD
            End If
        Next
    End Sub

    Function IsSuperiorPorcionMaule(Vol As Double) As Boolean
        If (USE_EXTRAO_AS_LOW) Then
            Return Vol >= LMAULE_EXTRAO_VOL
        Else
            Return Vol >= LMAULE_EXTRAO_VOL + LMAULE_INTER_VOL
        End If
    End Function

    Function IsLowePorcionMaule(Vol As Double) As Boolean
        If (USE_EXTRAO_AS_LOW) Then
            Return Vol < LMAULE_EXTRAO_VOL
        Else
            Return Vol < LMAULE_EXTRAO_VOL + LMAULE_INTER_VOL
        End If
    End Function

    Function GetLowerPorcionVolume() As Double
        If (USE_EXTRAO_AS_LOW) Then
            Return LMAULE_EXTRAO_VOL
        Else
            Return LMAULE_INTER_VOL
        End If
    End Function

    Function IsSuperiorPorcionRalco(Vol As Double) As Boolean
        Return Vol >= RALCO_INTER_VOL
    End Function

    Function IsLowerPorcionRalco(Vol As Double) As Boolean
        Return Vol < RALCO_INTER_VOL
    End Function

End Module
