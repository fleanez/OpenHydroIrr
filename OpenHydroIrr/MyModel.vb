Imports EEPHASE
Imports EEUTILITY.Enums
Imports EEUTILITY.Functions


Public Class MyModel
    Implements IOpenModel

    'Constants:
    Private Const USE_ARCHIVE_FOR_MAULE As Boolean = False
    Private Const USE_ARCHIVE_FOR_LAJA_INFLOW As Boolean = False
    Private Const USE_ARCHIVE_FOR_LAJA_MAXVOLUME As Boolean = False

    Private Shared m_vol30NovPerSample As Volume30Nov 'Volumes at Nov 30 in 1000m3
    Private Shared m_stepPeriod30Nov As Integer = -1
    Private Shared m_horizonPeriod30Nov As Integer = -1
    Private Shared m_periodInitMaule As Integer = -1
    Private Shared m_isValidIrr As Boolean = False
    Private Shared m_isInitialized As Boolean = False

    'Laja Storages:
    Private Shared stoElToro As Storage
    Private Shared stoLaja_Gen As Storage
    Private Shared stoLaja_Riego As Storage
    Private Shared stoLaja_Mixto As Storage

    'Maule Storages
    Private Shared stoLMaule As Storage
    Private Shared stoCipreses As Storage
    Private Shared stoInvernada_Eco As Storage
    Private Shared stoMauleLow_Eco_Gen As Storage
    Private Shared stoMauleLow_Eco_Riego As Storage
    Private Shared stoMauleSup_Eco_Gen As Storage
    Private Shared stoMauleSup_Eco_Riego As Storage

    'Other Storages
    Private Shared stoRalco As Storage
    Private Shared stoColbun As Storage

    'Variables:
    Private Shared varQInvFree As Variable
    Private Shared varQFZ As Variable
    Private Shared varVol30Nov As Variable
    Private Shared varReserveMinVolRalco As Variable
    Private Shared varReserveMinVolColbun As Variable
    Private Shared varReserveMinVolElToro As Variable

    Public Sub AfterInitialize() Implements IOpenModel.AfterInitialize

        If (CurrentModel.SimulationPhase = SimulationPhaseEnum.STSchedule) Then
            InitializePhase()
        End If

    End Sub

    Public Sub BeforeProperties() Implements IOpenModel.BeforeProperties
        Dim strMessage As String = ""

        InitializeStep()
        m_isValidIrr = IsValidIrrigation(strMessage)

        If m_isValidIrr Then
            CurrentModel.Feedback.LogMessage("Loading Irrigation Agreement pre-calculation...")
        Else
            Return
        End If

        Dim dScalar As Double
        Dim dateBeginHorizon As Date = Date.FromOADate(CurrentModel.Horizon(HorizonAttributeEnum.DateFrom))
        Dim nDayBeginning As Integer = CInt(CurrentModel.Horizon.Attribute(HorizonAttributeEnum.DayBeginning))
        Dim nPeriodsPerDayInLookAhead As Integer = CInt(CurrentModel.Horizon(HorizonAttributeEnum.LookaheadPeriodsperDay))
        Dim nPeriodsInHorizon As Integer = CurrentModel.Horizon.PeriodCount(PeriodEnum.Interval)
        Dim dateCur As Date
        Dim nHoursPerInterval As Double = CurrentModel.Horizon.HoursperInterval
        Dim nHoursPerIntervalInLookAhead As Double = CurrentModel.Horizon.HoursperInterval * CurrentModel.Horizon.m_nLookAheadRatio
        Dim curLookAheadPeriod As Integer
        Dim dateBeginStep As Date = dateBeginHorizon.AddHours((CurrentModel.Steps.CurrentStep - 1) * CurrentModel.Horizon.ChronoSansLookaheadHoursperStep)
        Dim dateBeginLookAhead As Date = dateBeginStep.AddHours(CurrentModel.Horizon.ChronoSansLookaheadHoursperStep)
        Dim nFirstPeriod As Integer = CurrentModel.Steps.FirstPeriod
        Dim nLastPeriod As Integer = CurrentModel.Steps.StepPeriodCount
        Dim nBeginInterval As Integer = CurrentModel.Steps.FirstIntervalInPeriod(nFirstPeriod)
        Dim nLastInterval As Integer = CurrentModel.Steps.LastIntervalInPeriod(nLastPeriod)
        Dim nTotalPeriods As Integer = CurrentModel.Steps.StepPeriodCount
        Dim dLajaNov30Vol As Double 'Lajas volume Nov 30th in 1000m3

        'Initialize volumes on Nov 30th (this is done just in the first step):
        If CurrentModel.Steps.IsFirstStep Then
            m_vol30NovPerSample.Volume(Multivariate.CurrentSample) = varVol30Nov.SampleValue(CurrentModel.Steps.FirstPeriod, PeriodEnum.Interval) * CMD2M3 'We store as 1000m3
        End If

        For nCurPeriod As Integer = 1 To CurrentModel.Steps.StepPeriodCount

            If (nCurPeriod <= CurrentModel.Steps.StepPeriodCountSansLookahead) Then
                'dateCur = EEUTILITY.Functions.Period2Date(CurrentModel.Steps.PeriodTypeFrom(PeriodEnum.Interval) + nCurPeriod - 1, CurrentModel.Horizon.PeriodsperDay, nHoursPerInterval, dateBeginStep, nDayBeginning)
                dateCur = EEUTILITY.Functions.Period2Date(nCurPeriod, CurrentModel.Horizon.PeriodsperDay, nHoursPerInterval, dateBeginStep, nDayBeginning)
            Else
                curLookAheadPeriod = nCurPeriod - CurrentModel.Steps.StepPeriodCountSansLookahead
                dateCur = EEUTILITY.Functions.Period2Date(curLookAheadPeriod, nPeriodsPerDayInLookAhead, nHoursPerIntervalInLookAhead, dateBeginLookAhead, nDayBeginning)
            End If

            If Utils.IsLajaInitDate(dateCur) Then

                m_stepPeriod30Nov = nCurPeriod
                dLajaNov30Vol = m_vol30NovPerSample.Volume(Multivariate.CurrentSample)

                Dim inflowGen(nPeriodsInHorizon) As Double
                Dim inflowRie(nPeriodsInHorizon) As Double
                Dim inflowMix(nPeriodsInHorizon) As Double

                Dim Flow2Vol As Double = 3.6 * CurrentModel.Steps.HoursinPeriod(nCurPeriod)

                If (dLajaNov30Vol > 1900000.0) Then 'Colchon Superior

                    inflowGen(m_horizonPeriod30Nov) = Math.Max(Math.Min((280500 + 0.65 * (dLajaNov30Vol - 1900000)) / Flow2Vol, Utils.LAJA_MAX_EXT_GEN), 0.0)
                    inflowRie(m_horizonPeriod30Nov) = Math.Max(880000 + 0.25 * (dLajaNov30Vol - 1900000) / Flow2Vol, 0.0)
                    inflowMix(m_horizonPeriod30Nov) = 0

                ElseIf (1370000 < dLajaNov30Vol AndAlso dLajaNov30Vol <= 1900000.0) Then 'Colchon Intermedio

                    inflowGen(m_horizonPeriod30Nov) = Math.Max(Math.Min((68500 + 0.04 * (dLajaNov30Vol - 1370000)) / Flow2Vol, Utils.LAJA_MAX_EXT_GEN), 0.0)
                    inflowRie(m_horizonPeriod30Nov) = Math.Max((668000 + 0.4 * (dLajaNov30Vol - 1370000)) / Flow2Vol, 0.0)
                    inflowMix(m_horizonPeriod30Nov) = 0

                ElseIf (1200000.0 < dLajaNov30Vol AndAlso dLajaNov30Vol <= 1370000) Then 'Colchon Transicion

                    inflowGen(m_horizonPeriod30Nov) = Math.Max(Math.Min((60000 + 0.05 * (dLajaNov30Vol - 1200000)) / Flow2Vol, Utils.LAJA_MAX_EXT_GEN), 0.0)
                    inflowRie(m_horizonPeriod30Nov) = Math.Max((600000 + 0.4 * (dLajaNov30Vol - 1200000)) / Flow2Vol, 0.0)
                    inflowMix(m_horizonPeriod30Nov) = 0

                ElseIf (dLajaNov30Vol <= 1200000.0) Then 'Colchon Inferior
                    Dim dVolLajaMixto As Double = 0.2 * dLajaNov30Vol
                    inflowGen(m_horizonPeriod30Nov) = Math.Max(Math.Min((0.05 * dLajaNov30Vol + 30000 - dVolLajaMixto) / Flow2Vol, Utils.LAJA_MAX_EXT_GEN), 0.0)
                    inflowRie(m_horizonPeriod30Nov) = Math.Max((570000 - dVolLajaMixto) / Flow2Vol, 0.0)
                    inflowMix(m_horizonPeriod30Nov) = dVolLajaMixto / Flow2Vol
                End If

#If USE_ARCHIVE_FOR_LAJA_INFLOW Then
                    Dim bHasNaturalInflows As Boolean = stoLaja_Gen.HasNaturalInflows And stoLaja_Riego.HasNaturalInflows And stoLaja_Mixto.HasNaturalInflows
                    If Not bHasNaturalInflows Then
                        CurrentModel.Feedback.LogMessage("Virtual Inflows without 'zero' inflows. Pre-calculation will exit")
                        m_isValidIrr = False
                        Return
                    End If
                    stoLaja_Gen.PutData(SystemStoragesEnum.NaturalInflow, inflowGen, PeriodEnum.Interval)
                    stoLaja_Riego.PutData(SystemStoragesEnum.NaturalInflow, inflowRie, PeriodEnum.Interval)
                    stoLaja_Mixto.PutData(SystemStoragesEnum.NaturalInflow, inflowMix, PeriodEnum.Interval)
#End If

                'Now we rebuild Vmax:
                Dim lg As New List(Of Double)
                Dim lr As New List(Of Double)
                Dim lm As New List(Of Double)
                For p As Integer = 1 To nPeriodsInHorizon
                    'For Each i As Integer In CurrentModel.Horizon.PeriodTypeIntervalTo(PeriodEnum.Interval)
                    If p < m_horizonPeriod30Nov Then
                        lg.Add(stoLaja_Gen.InitialVolume(nFirstPeriod) / Utils.CMD2M3) 'we need to convert to CMD
                        lr.Add(stoLaja_Riego.InitialVolume(nFirstPeriod) / Utils.CMD2M3)
                        lm.Add(stoLaja_Mixto.InitialVolume(nFirstPeriod) / Utils.CMD2M3)
                    Else
                        lg.Add(inflowGen(m_horizonPeriod30Nov) * Flow2Vol / Utils.CMD2M3) 'we need to convert to CMD
                        lr.Add(inflowRie(m_horizonPeriod30Nov) * Flow2Vol / Utils.CMD2M3) 'we need to convert to CMD
                        lm.Add(inflowMix(m_horizonPeriod30Nov) * Flow2Vol / Utils.CMD2M3) 'we need to convert to CMD
                    End If
                    'Next
                Next

#If USE_ARCHIVE_FOR_LAJA_MAXVOLUME Then
                stoLaja_Gen.PutData(SystemStoragesEnum.MaxVolume, lg.ToArray(), PeriodEnum.Interval)
                stoLaja_Riego.PutData(SystemStoragesEnum.MaxVolume, lr.ToArray(), PeriodEnum.Interval)
                stoLaja_Mixto.PutData(SystemStoragesEnum.MaxVolume, lm.ToArray(), PeriodEnum.Interval)
#End If
                stoLaja_Gen.MarkInputDirty(False)
                stoLaja_Riego.MarkInputDirty(False)
                stoLaja_Mixto.MarkInputDirty(False)
            End If

            If Utils.IsMauleInitDate(dateCur) Then
                m_periodInitMaule = nCurPeriod
            End If

#If USE_ARCHIVE_FOR_MAULE Then
            Dim dInitialVolumeLMAULE As Double
            If Utils.IsMauleInitDate(dateCur) Then
                m_periodInitMaule = nCurPeriod
                dScalar = stoLMaule.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod)
                dInitialVolumeLMAULE = stoLMaule.InitialVolume(nFirstPeriod)
                Dim inflowSupGen(nTotalPeriods) As Double
                Dim inflowSupRie(nTotalPeriods) As Double
                Dim inflowIntGen(nTotalPeriods) As Double
                Dim inflowIntRie(nTotalPeriods) As Double

                'Artificial inflows to fill up the superior porcion virtual storages:
                inflowSupGen(1) = 250000 / dScalar
                inflowSupRie(1) = 800000 / dScalar
                inflowIntGen(1) = 0.2 * (Math.Min(dInitialVolumeLMAULE, LMAULE_INTER_VOL) - 170000) / dScalar
                inflowIntRie(1) = 0.8 * (Math.Min(dInitialVolumeLMAULE, LMAULE_INTER_VOL) - 170000) / dScalar

                stoMauleSup_Eco_Gen.PutData(SystemStoragesEnum.NaturalInflow, inflowSupGen, PeriodEnum.Interval, 1, nCurPeriod, nCurPeriod)
                stoMauleSup_Eco_Riego.PutData(SystemStoragesEnum.NaturalInflow, inflowSupRie, PeriodEnum.Interval, 1, nCurPeriod, nCurPeriod)
                stoMauleLow_Eco_Gen.PutData(SystemStoragesEnum.NaturalInflow, inflowIntGen, PeriodEnum.Interval, 1, nCurPeriod, nCurPeriod)
                stoMauleLow_Eco_Riego.PutData(SystemStoragesEnum.NaturalInflow, inflowIntRie, PeriodEnum.Interval, 1, nCurPeriod, nCurPeriod)
                stoMauleSup_Eco_Gen.MarkInputDirty(False)
                stoMauleSup_Eco_Riego.MarkInputDirty(False)
                stoMauleLow_Eco_Gen.MarkInputDirty(False)
                stoMauleLow_Eco_Riego.MarkInputDirty(False)

                'Max Volume all virtual storages:
                Dim lsupg As New List(Of Double)
                Dim lsupr As New List(Of Double)
                Dim lintg As New List(Of Double)
                Dim lintr As New List(Of Double)
                Dim linv As New List(Of Double)
                Dim dateP As Date
                For p As Integer = 1 To CurrentModel.Horizon.PeriodCount(PeriodEnum.Interval)
                    dateP = EEUTILITY.Functions.Period2Date(p, CurrentModel.Horizon.PeriodsperDay, nHoursPerInterval, dateBeginHorizon, nDayBeginning)
                    lsupg.Add(350000 / Utils.CMD2M3) 'we need to convert to CMD
                    lsupr.Add(800000 / Utils.CMD2M3) 'we need to convert to CMD
                    If dateP.CompareTo(dateCur) = 0 Then
                        lintg.Add(inflowIntGen(1) * dScalar / Utils.CMD2M3) 'we need to convert to CMD
                        lintr.Add(inflowIntRie(1) * dScalar / Utils.CMD2M3) 'we need to convert to CMD
                        linv.Add(0.0) 'Reset invernada economies
                    Else
                        lintg.Add(stoMauleLow_Eco_Gen.MaxVolume(nFirstPeriod) / Utils.CMD2M3) 'we need to convert to CMD
                        lintr.Add(stoMauleLow_Eco_Riego.MaxVolume(nFirstPeriod) / Utils.CMD2M3) 'we need to convert to CMD
                        linv.Add(stoInvernada_Eco.MaxVolume(nFirstPeriod) / Utils.CMD2M3) 'we need to convert to CMD
                    End If
                Next
                stoMauleSup_Eco_Gen.PutData(SystemStoragesEnum.MaxVolume, lsupg.ToArray(), PeriodEnum.Interval)
                stoMauleSup_Eco_Riego.PutData(SystemStoragesEnum.MaxVolume, lsupr.ToArray(), PeriodEnum.Interval)
                stoMauleLow_Eco_Gen.PutData(SystemStoragesEnum.MaxVolume, lintg.ToArray(), PeriodEnum.Interval)
                stoMauleLow_Eco_Riego.PutData(SystemStoragesEnum.MaxVolume, lintr.ToArray(), PeriodEnum.Interval)
                stoInvernada_Eco.PutData(SystemStoragesEnum.MaxVolume, linv.ToArray(), PeriodEnum.Interval)

                stoMauleSup_Eco_Gen.MarkInputDirty(False)
                stoMauleSup_Eco_Riego.MarkInputDirty(False)
                stoMauleLow_Eco_Gen.MarkInputDirty(False)
                stoMauleLow_Eco_Riego.MarkInputDirty(False)
                stoInvernada_Eco.MarkInputDirty(False)
            End If
#End If

        Next

    End Sub

    Public Sub AfterProperties() Implements IOpenModel.AfterProperties
        If Not m_isValidIrr Then
            Return
        End If

        Dim dInitialVolumeELTORO As Double
        Dim dInitialVolumeLMAULE As Double
        Dim dInitialVolumeCIPRESES As Double
        Dim dInitialVolumeCOLBUN As Double
        Dim dInitialVolumeRALCO As Double

        Dim nInitPeriod As Double = CurrentModel.Steps.FirstPeriod
        Dim nTotalPeriods As Double = CurrentModel.Steps.StepPeriodCount

        'Initial Step (Volumes):
        For Each s In StoragesIN
            If s.Name.Equals("ELTORO", StringComparison.OrdinalIgnoreCase) Then
                dInitialVolumeELTORO = s.CurrentStorageStepBridge.PeriodZeroVolume
            ElseIf s.Name.Equals("LMAULE", StringComparison.OrdinalIgnoreCase) Then
                dInitialVolumeLMAULE = s.CurrentStorageStepBridge.PeriodZeroVolume
            ElseIf s.Name.Equals("CIPRESES", StringComparison.OrdinalIgnoreCase) Then
                dInitialVolumeCIPRESES = s.CurrentStorageStepBridge.PeriodZeroVolume
            ElseIf s.Name.Equals("COLBUN", StringComparison.OrdinalIgnoreCase) Then
                dInitialVolumeCOLBUN = s.CurrentStorageStepBridge.PeriodZeroVolume
            ElseIf s.Name.Equals("RALCO", StringComparison.OrdinalIgnoreCase) Then
                dInitialVolumeRALCO = s.CurrentStorageStepBridge.PeriodZeroVolume
            End If
        Next

        Dim dRiego(nTotalPeriods) As Double
        Dim dHoyaInt(nTotalPeriods) As Double
        For Each v As Variable In VariablesIN.InServiceObjects
            'Total Irrigation Maule:
            If Utils.isRiegoVariable(v) Then
                For nCurPeriod As Integer = 1 To CurrentModel.Steps.StepPeriodCount
                    dRiego(nCurPeriod) += v(SystemVariablesEnum.Profile, nCurPeriod)
                Next
            End If
            'HoyaIntermedia Maule:
            If Utils.IsHoyaIntermedia(v) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    dHoyaInt(nCurPeriod) += v(SystemVariablesEnum.Profile, nCurPeriod)
                Next
            End If

        Next

        Dim varInflowInvEco As DecisionVariable
        Dim varInflowMauleIEcoGen As DecisionVariable
        Dim varInflowMauleIEcoRiego As DecisionVariable
        Dim varInflowMauleSEcoGen As DecisionVariable
        Dim varInflowMauleSEcoRiego As DecisionVariable
        For Each v As DecisionVariable In DecisionVariablesIN.InServiceObjects
            If v.Name.Equals("Inflow_Invernada_Eco", StringComparison.OrdinalIgnoreCase) Then
                varInflowInvEco = v
            ElseIf v.Name.Equals("Inflow_MauleLow_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then
                varInflowMauleIEcoGen = v
            ElseIf v.Name.Equals("Inflow_MauleLow_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then
                varInflowMauleIEcoRiego = v
            ElseIf v.Name.Equals("Inflow_MauleSup_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then
                varInflowMauleSEcoGen = v
            ElseIf v.Name.Equals("Inflow_MauleSup_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then
                varInflowMauleSEcoRiego = v
            End If
        Next

        If IsNothing(varInflowInvEco) OrElse IsNothing(varInflowMauleIEcoGen) OrElse IsNothing(varInflowMauleIEcoRiego) _
            OrElse IsNothing(varInflowMauleSEcoGen) OrElse IsNothing(varInflowMauleSEcoRiego) Then
            CurrentModel.Feedback.LogMessage("Couldn't find all required decision variables for Maule...") 'TODO: Move this to validation
            Return
        End If

        'Total Deficit:
        Dim dDeficit(nTotalPeriods) As Double
        For nCurPeriod As Integer = 1 To CurrentModel.Steps.StepPeriodCount
            dDeficit(nCurPeriod) = dRiego(nCurPeriod) - dHoyaInt(nCurPeriod) ' stoLMaule(SystemStoragesEnum.NaturalInflow, nCurPeriod)
        Next

        'Filtration constraints:
        For Each c In ConstraintsIN.InServiceObjects
            If (Utils.IsFiltration(c)) Then
                Dim s As Storage
                Dim dFiltRHS As Double
                Dim dFiltCoeff As Double
                s = Utils.GetStorage(c)
                dFiltCoeff = Utils.GetFiltrationLinearCoefficient(c.Name, s.InitialVolume(nInitPeriod))
                dFiltRHS = Utils.GetFiltrationLinearRHS(c.Name, s.InitialVolume(nInitPeriod))
                For nCurPeriod As Integer = 1 To CurrentModel.Steps.StepPeriodCount
                    s.EndVolumeCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = dFiltCoeff
                    c.ConstraintRow.RHS(nCurPeriod) = dFiltRHS
                Next
            End If
        Next

        'Adjust FCF constraints to the end of the lookahead:
        For Each c In ConstraintsIN.InServiceObjects
            If c.Name.StartsWith("FCF_", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To CurrentModel.Steps.PeriodTypeCount(c.PeriodType) - 1
                    c.ConstraintRow.RHS(nCurPeriod) = Utils.FCF_MIN_VALUE
                Next
            End If
        Next

        'Maule Irrigation Constraints:
        Dim dScalar As Double
        For Each c In ConstraintsIN.InServiceObjects

            If c.Name.Equals("ExtractDef_Maule", StringComparison.OrdinalIgnoreCase) Then
                'c.Initialize(0)
                For nCurPeriod As Integer = 1 To CurrentModel.Steps.StepPeriodCount
                    dScalar = stoLMaule.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod) 'This is to convert to flow
                    'stoLMaule.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = 100.0 / dScalar
                    If Utils.IsSuperiorPorcionMaule(dInitialVolumeLMAULE) Then
                        stoMauleSup_Eco_Gen.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = -100.0 / dScalar
                        stoMauleSup_Eco_Riego.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = -100.0 / dScalar
                        stoInvernada_Eco.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = -100.0 / dScalar
                        stoMauleLow_Eco_Gen.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = 0.0
                        stoMauleLow_Eco_Riego.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = 0.0
                    Else
                        stoMauleSup_Eco_Gen.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = 0.0
                        stoMauleSup_Eco_Riego.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = 0.0
                        stoInvernada_Eco.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = 0.0
                        stoMauleLow_Eco_Gen.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = -100.0 / dScalar
                        stoMauleLow_Eco_Riego.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = -100.0 / dScalar
                    End If
                Next

            ElseIf c.Name.Equals("Maule_Irrigation_Deficit", StringComparison.OrdinalIgnoreCase) Then

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If dDeficit(nCurPeriod) > 0 Then
                        c.ConstraintRow(nCurPeriod).RHS = dDeficit(nCurPeriod)
                    End If
                Next

            ElseIf c.Name.Equals("Cipreses_IfDeficitMaule", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    dScalar = stoCipreses.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod) 'This is to convert N.I. into volume
                    varInflowInvEco.Coefficient(stoInvernada_Eco.BalanceConstraint(nCurPeriod), nCurPeriod) = 1.0 'This goes for sure
                    If dDeficit(nCurPeriod) > 0 AndAlso Utils.IsSuperiorPorcionMaule(dInitialVolumeLMAULE) Then 'Only in the superior portion Inv can accumulate economies
                        stoCipreses.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = -1.0
                        c.ConstraintRow(nCurPeriod).RHS = -stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) * dScalar
                    Else
                        stoCipreses.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = 0.0
                    End If
                Next

            ElseIf c.Name.Equals("Cipreses_IfDeficitMaule_NoDeficitInv_2", StringComparison.OrdinalIgnoreCase) Then

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If dDeficit(nCurPeriod) > 0 AndAlso stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) > dDeficit(nCurPeriod) Then
                        c.ConstraintRow(nCurPeriod).RHS = dDeficit(nCurPeriod)
                    End If
                Next

            ElseIf c.Name.Equals("Cipreses_IfDeficitMaule_IfDeficitInv_1", StringComparison.OrdinalIgnoreCase) Then

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If dDeficit(nCurPeriod) > 0 AndAlso stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) <= dDeficit(nCurPeriod) Then
                        c.ConstraintRow(nCurPeriod).RHS = dDeficit(nCurPeriod)
                    End If
                Next

            ElseIf c.Name.Equals("DeficitMaulexEcoRiego", StringComparison.OrdinalIgnoreCase) Then

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    dScalar = stoLMaule.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod)
                    If dDeficit(nCurPeriod) > 0 AndAlso stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) <= dDeficit(nCurPeriod) Then
                        stoLMaule.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = SafeDivision(-1.0, dScalar)
                        If (Utils.IsLowePorcionMaule(dInitialVolumeLMAULE)) Then
                            stoInvernada_Eco.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = SafeDivision(-1.0, dScalar)
                        Else
                            stoInvernada_Eco.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = 0.0
                        End If
                    Else
                        stoLMaule.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = 0.0
                    End If
                Next

            ElseIf c.Name.Equals("Inv_CotaFija_ub", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) + dHoyaInt(nCurPeriod) < varQInvFree(SystemVariablesEnum.Profile, nCurPeriod) Then
                        'dScalar = stoCipreses.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod) 'This is to convert to flow
                        c.ConstraintRow(nCurPeriod).RHS = stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod)
                    End If
                Next

            ElseIf c.Name.Equals("Inv_CotaFija_lb", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) + dHoyaInt(nCurPeriod) < varQInvFree(SystemVariablesEnum.Profile, nCurPeriod) Then
                        'dScalar = stoCipreses.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod) 'This is to convert to flow
                        c.ConstraintRow(nCurPeriod).RHS = stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod)
                    End If
                Next

            ElseIf c.Name.Equals("HoyaIntermedia", StringComparison.OrdinalIgnoreCase) Then

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    c.ConstraintRow(nCurPeriod).RHS = dHoyaInt(nCurPeriod)
                Next

            ElseIf c.Name.Equals("Maule_Irrigation_Sup", StringComparison.OrdinalIgnoreCase) OrElse c.Name.Equals("Maule_Generation_Sup", StringComparison.OrdinalIgnoreCase) Then
                If Not (Utils.IsSuperiorPorcionMaule(dInitialVolumeLMAULE)) Then
                    c.ConstraintRow.RHS = 0
                End If

            ElseIf c.Name.Equals("Maule_Irrigation_Low", StringComparison.OrdinalIgnoreCase) OrElse c.Name.Equals("Maule_Generation_Low", StringComparison.OrdinalIgnoreCase) Then
                If Utils.IsSuperiorPorcionMaule(dInitialVolumeLMAULE) Then
                    c.ConstraintRow.RHS = 0
                End If

            ElseIf c.Name.Equals("InflowDef_Invernada_Eco", StringComparison.OrdinalIgnoreCase) Then
                If Utils.IsSuperiorPorcionMaule(dInitialVolumeLMAULE) Then
                    c.ConstraintRow.RHS = Utils.FREE_FLOW
                Else
                    'Shouldn't we set this back to zero otherwise? Seems not
                End If

            ElseIf c.Name.Equals("InflowDef_MauleLow_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    Dim dInflow As Double = 0.0
                    dScalar = stoMauleLow_Eco_Gen.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod)
                    varInflowMauleIEcoGen.Coefficient(stoMauleLow_Eco_Gen.BalanceConstraint(nCurPeriod), nCurPeriod) = 1.0
                    If Utils.IsLowePorcionMaule(dInitialVolumeLMAULE) Then
                        dInflow += 0.2 * stoLMaule(SystemStoragesEnum.NaturalInflow, nCurPeriod) * dScalar
                    End If
                    If nCurPeriod = m_periodInitMaule Then 'If it is initialize period we also need to add the 20% of the reserve
                        If Utils.IsLowePorcionMaule(dInitialVolumeLMAULE) Then
                            dInflow += 0.2 * Math.Max(dInitialVolumeLMAULE - Utils.LMAULE_EXTRAO_VOL, 0.0) 'Check negative should be temporarily
                        Else
                            dInflow += 0.2 * Utils.GetLowerPorcionVolume()
                        End If
                    End If
                    c.ConstraintRow.RHS(nCurPeriod) = dInflow
                Next

            ElseIf c.Name.Equals("InflowDef_MauleLow_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    Dim dInflow As Double = 0.0
                    dScalar = stoMauleLow_Eco_Riego.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod)
                    varInflowMauleIEcoRiego.Coefficient(stoMauleLow_Eco_Riego.BalanceConstraint(nCurPeriod), nCurPeriod) = 1.0
                    If Utils.IsLowePorcionMaule(dInitialVolumeLMAULE) Then
                        dInflow += 0.8 * stoLMaule(SystemStoragesEnum.NaturalInflow, nCurPeriod) * dScalar
                    End If
                    If nCurPeriod = m_periodInitMaule Then 'If it is initialize period we also need to add the 80% of the reserve
                        If Utils.IsLowePorcionMaule(dInitialVolumeLMAULE) Then
                            dInflow += 0.8 * Math.Max(dInitialVolumeLMAULE - Utils.LMAULE_EXTRAO_VOL, 0.0) 'Check negative should be temporarily
                        Else
                            dInflow += 0.8 * Utils.GetLowerPorcionVolume()
                        End If
                    End If
                    c.ConstraintRow.RHS(nCurPeriod) = dInflow
                Next

            ElseIf c.Name.Equals("InflowDef_MauleSup_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then

                If m_periodInitMaule > 0 Then
                    dScalar = stoLMaule.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(m_periodInitMaule)
                    varInflowMauleSEcoGen.Coefficient(stoMauleSup_Eco_Gen.BalanceConstraint(m_periodInitMaule), m_periodInitMaule) = 1.0
                    c.ConstraintRow.RHS(m_periodInitMaule) = stoMauleSup_Eco_Gen.MaxVolume(nInitPeriod) 'Previously 250000
                End If

            ElseIf c.Name.Equals("InflowDef_MauleSup_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then

                If m_periodInitMaule > 0 Then
                    dScalar = stoLMaule.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(m_periodInitMaule)
                    varInflowMauleSEcoRiego.Coefficient(stoMauleSup_Eco_Riego.BalanceConstraint(m_periodInitMaule), m_periodInitMaule) = 1.0
                    c.ConstraintRow.RHS(m_periodInitMaule) = stoMauleSup_Eco_Riego.MaxVolume(nInitPeriod) ' Previously 800000
                End If

            End If

        Next

        'Ralco constraints:
        Dim dMaxVolume As Double
        For Each c In ConstraintsIN.InServiceObjects
            dMaxVolume = stoRalco(SystemStoragesEnum.MaxVolume, nInitPeriod) * CMD2M3
            If c.Name.Equals("Extraction_Ralco_Sup", StringComparison.OrdinalIgnoreCase) Then
                If Utils.IsSuperiorPorcionRalco(dInitialVolumeRALCO) Then
                    c.ConstraintRow.RHS = 3337 + 0.00808552 * dInitialVolumeRALCO
                Else
                    c.ConstraintRow.RHS = Utils.FREE_RALCO
                End If
            ElseIf c.Name.Equals("Extraction_Ralco_Inf", StringComparison.OrdinalIgnoreCase) Then
                If Utils.IsLowerPorcionRalco(dInitialVolumeRALCO) Then
                    c.ConstraintRow.RHS = 1375 + 0.00602351 * dInitialVolumeRALCO
                Else
                    c.ConstraintRow.RHS = Utils.FREE_RALCO
                End If
            End If
        Next

        'Reserve volume constraints:
        For Each c In ConstraintsIN.InServiceObjects
            If c.Name.Equals("ReserveMinVol_Ralco", StringComparison.OrdinalIgnoreCase) Then
                If (dInitialVolumeRALCO <= varReserveMinVolRalco(SystemVariablesEnum.Profile, nInitPeriod) * CMD2M3) Then
                    c.ConstraintRow.RHS = 0.0
                Else
                    c.ConstraintRow.RHS = Utils.FREE_FLOW
                End If
            ElseIf c.Name.Equals("ReserveMinVol_ElToro", StringComparison.OrdinalIgnoreCase) Then
                If (dInitialVolumeELTORO <= varReserveMinVolElToro(SystemVariablesEnum.Profile, nInitPeriod) * CMD2M3) Then
                    c.ConstraintRow.RHS = 0.0
                Else
                    c.ConstraintRow.RHS = Utils.FREE_FLOW
                End If
            ElseIf c.Name.Equals("ReserveMinVol_Colbun", StringComparison.OrdinalIgnoreCase) Then
                If (dInitialVolumeCOLBUN <= varReserveMinVolColbun(SystemVariablesEnum.Profile, nInitPeriod) * CMD2M3) Then
                    c.ConstraintRow.RHS = 0.0
                Else
                    c.ConstraintRow.RHS = Utils.FREE_FLOW
                End If
            End If
        Next

    End Sub

    Public Sub BeforeOptimize() Implements IOpenModel.BeforeOptimize
    End Sub

    Public Sub AfterOptimize() Implements IOpenModel.AfterOptimize
    End Sub

    Public Sub BeforeRecordSolution() Implements IOpenModel.BeforeRecordSolution
        'We register the new 30 Nov Volume
        For Each s In StoragesIN
            If s.Name.Equals("ELTORO", StringComparison.OrdinalIgnoreCase) Then
                If (m_stepPeriod30Nov > 0) Then
                    'Dim d(CurrentModel.Steps.StepPeriodCount) As Double
                    'm_vol30Nov = s.EndVolume.DirtyValue(m_period30Nov)
                    m_vol30NovPerSample.Volume(Multivariate.CurrentSample) = s.EndVolume(m_stepPeriod30Nov)
                    'm_vol30Nov = s.EndVolume(m_stepPeriod30Nov)
                    's.GetEndVolumePrimalList(d)
                    'm_vol30Nov = s.EndVolumeOUT(m_period30Nov)
                End If
            End If
            If s.Name.Equals("Laja_Gen", StringComparison.OrdinalIgnoreCase) Then
                'Dim d(CurrentModel.Steps.StepPeriodCount) As Double
                's.GetEndVolumePrimalList(d)
                'Dim endVLajaGen As Double = s.EndVolume(CurrentModel.Steps.StepPeriodCountSansLookahead)
            End If
        Next
    End Sub

    Public Sub AfterRecordSolution() Implements IOpenModel.AfterRecordSolution
    End Sub

    Public Sub TerminatePhase() Implements IOpenModel.TerminatePhase
    End Sub

    Public Function OnWarning(Message As String) As Boolean Implements IOpenModel.OnWarning
        Return False
    End Function

    Public Function EnforceMyConstraints() As Integer Implements IOpenModel.EnforceMyConstraints
        Return 0
    End Function

    Public Function HasDynamicTransmissionConstraints() As Boolean Implements IOpenModel.HasDynamicTransmissionConstraints
        Return 0
    End Function

    Private Shared Sub InitializePhase()
        Dim dateBeginHorizon As Date = Date.FromOADate(CurrentModel.Horizon(HorizonAttributeEnum.DateFrom))
        Dim nDayBeginning As Integer = CInt(CurrentModel.Horizon.Attribute(HorizonAttributeEnum.DayBeginning))
        Dim dateCur As Date

        m_vol30NovPerSample = New Volume30Nov(Multivariate.SampleCount)

        'Initilize the period of the horizon for Nov 30th:
        If (m_horizonPeriod30Nov < 0) Then
            For nPeriod As Integer = 1 To CurrentModel.Horizon.PeriodCount(PeriodEnum.Interval)
                dateCur = EEUTILITY.Functions.Period2Date(nPeriod, CurrentModel.Horizon.PeriodsperDay, CurrentModel.Horizon.HoursperInterval, dateBeginHorizon, nDayBeginning)
                If (Utils.IsLajaInitDate(dateCur)) Then
                    m_horizonPeriod30Nov = nPeriod
                    Return
                End If
            Next
        End If
    End Sub

    Private Sub InitializeStep()

        Dim nInitPeriod As Integer = CurrentModel.Steps.FirstPeriod
        Dim bIsFirstStep As Boolean = CurrentModel.Steps.IsFirstStep
        m_periodInitMaule = -1
        m_stepPeriod30Nov = -1

        For Each s In StoragesIN
            If s.Name.Equals("ELTORO", StringComparison.OrdinalIgnoreCase) Then
                stoElToro = s
            ElseIf s.Name.Equals("Laja_Gen", StringComparison.OrdinalIgnoreCase) Then
                stoLaja_Gen = s
            ElseIf s.Name.Equals("Laja_Riego", StringComparison.OrdinalIgnoreCase) Then
                stoLaja_Riego = s
            ElseIf s.Name.Equals("Laja_Mixto", StringComparison.OrdinalIgnoreCase) Then
                stoLaja_Mixto = s
            ElseIf s.Name.Equals("LMAULE", StringComparison.OrdinalIgnoreCase) Then
                stoLMaule = s
            ElseIf s.Name.Equals("CIPRESES", StringComparison.OrdinalIgnoreCase) Then
                stoCipreses = s
            ElseIf s.Name.Equals("Invernada_Eco", StringComparison.OrdinalIgnoreCase) Then
                stoInvernada_Eco = s
            ElseIf s.Name.Equals("MauleLow_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then
                stoMauleLow_Eco_Gen = s
            ElseIf s.Name.Equals("MauleLow_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then
                stoMauleLow_Eco_Riego = s
            ElseIf s.Name.Equals("MauleSup_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then
                stoMauleSup_Eco_Gen = s
            ElseIf s.Name.Equals("MauleSup_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then
                stoMauleSup_Eco_Riego = s
            ElseIf s.Name.Equals("RALCO", StringComparison.OrdinalIgnoreCase) Then
                stoRalco = s
            ElseIf s.Name.Equals("COLBUN", StringComparison.OrdinalIgnoreCase) Then
                stoColbun = s
            End If
        Next

        For Each v As Variable In VariablesIN.InServiceObjects
            If (v.Name.Equals("LAJAVol30Nov")) Then
                varVol30Nov = v
                'If bIsFirstStep Then
                '    m_vol30NovPerSample.Volume(Multivariate.CurrentSample) = v.SampleValue(CurrentModel.Steps.FirstPeriod, PeriodEnum.Interval) * CMD2M3 'We store as 1000m3
                'End If
                'If m_vol30Nov = 0 Then
                '    m_vol30Nov = v.ArcValue(SystemVariablesEnum.Profile, 1)
                'End If
            ElseIf v.Name.Equals("QFZ", StringComparison.OrdinalIgnoreCase) Then
                varQFZ = v
            ElseIf v.Name.Equals("QInvFree", StringComparison.OrdinalIgnoreCase) Then 'Cipreses min inflow for free operation:
                varQInvFree = v
            ElseIf v.Name.Equals("ReserveMinVolRalco", StringComparison.OrdinalIgnoreCase) Then
                varReserveMinVolRalco = v
            ElseIf v.Name.Equals("ReserveMinVolElToro", StringComparison.OrdinalIgnoreCase) Then
                varReserveMinVolElToro = v
            ElseIf v.Name.Equals("ReserveMinVolColbun", StringComparison.OrdinalIgnoreCase) Then
                varReserveMinVolColbun = v
            End If
        Next

    End Sub

    Private Shared Function IsValidIrrigation(ByRef strMessage As String) As Boolean
        Dim bIsNotValidLaja As Boolean
        Dim bIsNotValidMaule As Boolean
        Dim bIsNotValidReserveMin As Boolean
        Dim bIsNotValidRalco As Boolean
        Dim bIsNotValidColbun As Boolean

        bIsNotValidLaja = IsNothing(stoElToro) Or IsNothing(stoLaja_Gen) Or IsNothing(stoLaja_Gen) Or IsNothing(stoLaja_Mixto) Or IsNothing(stoLaja_Riego) _
                    Or IsNothing(varQFZ)
        bIsNotValidMaule = IsNothing(stoLMaule) Or IsNothing(stoCipreses) Or IsNothing(stoInvernada_Eco) Or IsNothing(stoMauleLow_Eco_Gen) Or IsNothing(stoMauleLow_Eco_Riego) _
                    Or IsNothing(stoMauleSup_Eco_Gen) Or IsNothing(stoMauleSup_Eco_Riego) Or IsNothing(varQInvFree)
        bIsNotValidReserveMin = IsNothing(varReserveMinVolColbun) Or IsNothing(varReserveMinVolRalco) Or IsNothing(varReserveMinVolElToro)
        bIsNotValidRalco = IsNothing(stoRalco)
        bIsNotValidColbun = IsNothing(stoColbun)
        Return Not bIsNotValidLaja And Not bIsNotValidMaule And Not bIsNotValidReserveMin And Not bIsNotValidRalco And Not bIsNotValidColbun And CurrentModel.SimulationPhase = SimulationPhaseEnum.STSchedule
    End Function

End Class

Class Volume30Nov

    Private m_vol30Nov As List(Of Double)

    Public Sub New(Samples As Integer)
        m_vol30Nov = New List(Of Double)
        For i As Integer = 1 To Samples
            m_vol30Nov.Add(0.0)
        Next
    End Sub

    Public Property Volume(nSample As Integer)
        Get
            Return m_vol30Nov(nSample - 1)
        End Get
        Set
            m_vol30Nov(nSample - 1) = Value
        End Set
    End Property

End Class