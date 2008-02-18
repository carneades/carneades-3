(module uva-model mzscheme
  
  (require "../../src/argument-builtins.scm") 
  (require "../../src/rule.scm")
  (require "../../src/shell.scm")
  
  (define rb1 
    (rulebase
     
     ;"strict" terminological rules
     
     (rule* subClass_1
            (if	(Plan ?x)
                (Mental_Object ?x)))
     
     (rule* subClass_2
            (if	(Cancellation ?x)
                (Action ?x)))
     
     (rule* subClass_3
            (if	(Income ?x)
                (Financial_Role ?x)))
     
     (rule* subClass_4
            (if	(Provision ?x)
                (Tangible_Asset ?x)))
     
     (rule* subClass_5
            (if	(Forbidden ?x)
                (Normative_Propositional_Attitude ?x)))
     
     (rule* subClass_6
            (if	(Holding ?x)
                (Tangible_Asset ?x)))
     
     (rule* subClass_7
            (if	(Customary_Law ?x)
                (Custom ?x)))
     
     (rule* subClass_8
            (if	(European_Parliament ?x)
                (Parliament ?x)))
     
     (rule* subClass_9
            (if	(Limited_Company ?x)
                (Company ?x)))
     
     (rule* subClass_10
            (if	(Assignment ?x)
                (Legal_Speech_Act ?x)))
     
     (rule* subClass_11
            (if	(Allowed_And_Disallowed ?x)
                (Disallowed ?x)))
     
     (rule* subClass_12
            (if	(Communicated_Attitude ?x)
                (Propositional_Attitude ?x)))
     
     (rule* subClass_13
            (if	(Resolution ?x)
                (International_Agreement ?x)))
     
     (rule* subClass_14
            (if	(Share ?x)
                (Tangible_Asset ?x)))
     
     (rule* subClass_15
            (if	(Establishment ?x)
                (Legal_Person ?x)))
     
     (rule* subClass_16
            (if	(Mental_Object ?x)
                (Mental_Concept ?x)))
     
     (rule* subClass_17
            (if	(Dissolve ?x)
                (Action ?x)))
     
     (rule* subClass_18
            (if	(Method ?x)
                (Action ?x)))
     
     (rule* subClass_19
            (if	(European_Economic_Operation ?x)
                (Economic_Operation ?x)))
     
     (rule* subClass_20
            (if	(Acquire ?x)
                (Action ?x)))
     
     (rule* subClass_21
            (if	(Treaty ?x)
                (Legal_Document ?x)))
     
     (rule* subClass_22
            (if	(State ?x)
                (Legislative_Body ?x)))
     
     (rule* subClass_23
            (if	(Reaction ?x)
                (Action ?x)))
     
     (rule* subClass_24
            (if	(Withdraw ?x)
                (Action ?x)))
     
     (rule* subClass_25
            (if	(Financial_Role ?x)
                (Role ?x)))
     
     (rule* subClass_26
            (if	(Tax ?x)
                (Obligation ?x)))
     
     (rule* subClass_27
            (if	(Resolution ?x)
                (Soft_Law ?x)))
     
     (rule* subClass_28
            (if	(Social_Legal_Role ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_29
            (if	(Tax_Avoidance ?x)
                (Action ?x)))
     
     (rule* subClass_30
            (if	(Restriction ?x)
                (Norm ?x)))
     
     (rule* subClass_31
            (if	(Inform ?x)
                (rev_lkif_Action ?x)))
     
     (rule* subClass_32
            (if	(Exchange ?x)
                (rev_lkif_Action ?x)))
     
     (rule* subClass_33
            (if	(Jurisdiction ?x)
                (Area ?x)))
     
     (rule* subClass_34
            (if	(Liability ?x)
                (Economic_Role ?x)))
     
     (rule* subClass_35
            (if	(Lie ?x)
                (Assertion ?x)))
     
     (rule* subClass_36
            (if	(Properly_Constituted ?x)
                (Qualification ?x)))
     
     (rule* subClass_37
            (if	(Statute ?x)
                (Legal_Document ?x)))
     
     (rule* subClass_38
            (if	(Liquidation ?x)
                (Action ?x)))
     
     (rule* subClass_39
            (if	(Termination ?x)
                (Change ?x)))
     
     (rule* subClass_40
            (if	(rev_lkif_Action ?x)
                (Process ?x)))
     
     (rule* subClass_41
            (if	(Right ?x)
                (Norm ?x)))
     
     (rule* subClass_42
            (if	(Obligative_Right ?x)
                (Right ?x)))
     
     (rule* subClass_43
            (if	(Directive ?x)
                (Proclamation ?x)))
     
     (rule* subClass_44
            (if	(Surprise ?x)
                (Observation ?x)))
     
     (rule* subClass_45
            (if	(Reinstate ?x)
                (rev_lkif_Action ?x)))
     
     (rule* subClass_46
            (if	(Strictly_Allowed ?x)
                (Allowed ?x)))
     
     (rule* subClass_47
            (if	(Non-binding_International_Agreement ?x)
                (Soft_Law ?x)))
     
     (rule* subClass_48
            (if	(Moment ?x)
                (Temporal_Occurrence ?x)))
     
     (rule* subClass_49
            (if	(Organisation_Role ?x)
                (Social_Role ?x)))
     
     (rule* subClass_50
            (if	(Customary_Law ?x)
                (Legal_Source ?x)))
     
     (rule* subClass_51
            (if	(Cause ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_52
            (if	(Custom ?x)
                (Medium ?x)))
     
     (rule* subClass_53
            (if	(Economic_and_Social_Committee ?x)
                (Committee ?x)))
     
     (rule* subClass_54
            (if	(Value_Measure ?x)
                (Qualification ?x)))
     
     (rule* subClass_55
            (if	(Interval ?x)
                (Composition ?x)))
     
     (rule* subClass_56
            (if	(Capital ?x)
                (Financial_Role ?x)))
     
     (rule* subClass_57
            (if	(Business_Level ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_58
            (if	(Value_for_Tax_Purposes ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_59
            (if	(Economic_Role ?x)
                (Role ?x)))
     
     (rule* subClass_60
            (if	(Legal_Article ?x)
                (Medium ?x)))
     
     (rule* subClass_61
            (if	(Branch_of_Activity ?x)
                (Tangible_Asset ?x)))
     
     (rule* subClass_62
            (if	(Member_State ?x)
                (State ?x)))
     
     (rule* subClass_63
            (if	(Euro ?x)
                (Unit_of_Measure ?x)))
     
     (rule* subClass_64
            (if	(Tangible_Asset ?x)
                (Asset ?x)))
     
     (rule* subClass_65
            (if	(Spatio_Temporal_Occurrence ?x)
                (Occurrence ?x)))
     
     (rule* subClass_66
            (if	(Administrative_Provision ?x)
                (Provision ?x)))
     
     (rule* subClass_67
            (if	(Physical_Object ?x)
                (Physical_Concept ?x)))
     
     (rule* subClass_68
            (if	(Natural_Object ?x)
                (Physical_Object ?x)))
     
     (rule* subClass_69
            (if	(Fiscally_Transparent ?x)
                (Qualification ?x)))
     
     (rule* subClass_70
            (if	(Payment ?x)
                (Transfer ?x)))
     
     (rule* subClass_71
            (if	(Interval ?x)
                (Temporal_Occurrence ?x)))
     
     (rule* subClass_72
            (if	(Exclusionary_Right ?x)
                (Obligative_Right ?x)))
     
     (rule* subClass_73
            (if	(Place ?x)
                (Spatio_Temporal_Occurrence ?x)))
     
     (rule* subClass_74
            (if	(Foundation ?x)
                (Corporation ?x)))
     
     (rule* subClass_75
            (if	(Valid_Rule ?x)
                (Rule ?x)))
     
     (rule* subClass_76
            (if	(Proposal ?x)
                (Evaluative_Proposition ?x)))
     
     (rule* subClass_77
            (if	(Professional_Legal_Role ?x)
                (Organisation_Role ?x)))
     
     (rule* subClass_78
            (if	(Argument ?x)
                (Argument ?x)))
     
     (rule* subClass_79
            (if	(Absolute_Place ?x)
                (Place ?x)))
     
     (rule* subClass_80
            (if	(Exhausted_From_Tax_Purposes ?x)
                (Qualification ?x)))
     
     (rule* subClass_81
            (if	(Allotment ?x)
                (Distribution ?x)))
     
     (rule* subClass_82
            (if	(Transfer_of_the_Registered_Office ?x)
                (Transfer ?x)))
     
     (rule* subClass_83
            (if	(Legislative_Body ?x)
                (Public_Body ?x)))
     
     (rule* subClass_84
            (if	(Renounce ?x)
                (Action ?x)))
     
     (rule* subClass_85
            (if	(Normative_Propositional_Attitude ?x)
                (Propositional_Attitude ?x)))
     
     (rule* subClass_86
            (if	(Nominal_Capital ?x)
                (Capital ?x)))
     
     (rule* subClass_87
            (if	(Personal_Plan ?x)
                (Plan ?x)))
     
     (rule* subClass_88
            (if	(Co-operative ?x)
                (Society ?x)))
     
     (rule* subClass_89
            (if	(Problem ?x)
                (Observation ?x)))
     
     (rule* subClass_90
            (if	(Acquiring_Company ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_91
            (if	(Normatively_Qualified ?x)
                (Qualified ?x)))
     
     (rule* subClass_92
            (if	(Shareholder ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_93
            (if	(Delegation ?x)
                (Public_Act ?x)))
     
     (rule* subClass_94
            (if	(Expression ?x)
                (Proposition ?x)))
     
     (rule* subClass_95
            (if	(Territory ?x)
                (Area ?x)))
     
     (rule* subClass_96
            (if	(Prohibition ?x)
                (Permission ?x)))
     
     (rule* subClass_97
            (if	(Distribution ?x)
                (Action ?x)))
     
     (rule* subClass_98
            (if	(Norm ?x)
                (Qualification ?x)))
     
     (rule* subClass_99
            (if	(Legal_Speech_Act ?x)
                (Speech_Act ?x)))
     
     (rule* subClass_100
            (if	(Resident ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_101
            (if	(Public_Body ?x)
                (Legal_Person ?x)))
     
     (rule* subClass_102
            (if	(Tax_Provision ?x)
                (Provision ?x)))
     
     (rule* subClass_103
            (if	(Creation ?x)
                (Action ?x)))
     
     (rule* subClass_104
            (if	(Compute ?x)
                (Action ?x)))
     
     (rule* subClass_105
            (if	(Propositional_Attitude ?x)
                (Mental_Object ?x)))
     
     (rule* subClass_106
            (if	(Potestative_Right ?x)
                (Enabling_Power ?x)))
     
     (rule* subClass_107
            (if	(Desire ?x)
                (Propositional_Attitude ?x)))
     
     (rule* subClass_108
            (if	(Code_of_Conduct ?x)
                (Legal_Document ?x)))
     
     (rule* subClass_109
            (if	(Safeguarding ?x)
                (Action ?x)))
     
     (rule* subClass_110
            (if	(Argument ?x)
                (Reason ?x)))
     
     (rule* subClass_111
            (if	(Pair_Of_Periods ?x)
                (Pair ?x)))
     
     (rule* subClass_112
            (if	(Tax_Rules ?x)
                (Rule ?x)))
     
     (rule* subClass_113
            (if	(Intangible_Liability ?x)
                (Liability ?x)))
     
     (rule* subClass_114
            (if	(Decree ?x)
                (Proclamation ?x)))
     
     (rule* subClass_115
            (if	(Derogation ?x)
                (Exemption ?x)))
     
     (rule* subClass_116
            (if	(Represented_Action ?x)
                (Plan ?x)))
     
     (rule* subClass_117
            (if	(Process ?x)
                (Change ?x)))
     
     (rule* subClass_118
            (if	(Assumption ?x)
                (Assumption ?x)))
     
     (rule* subClass_119
            (if	(Public_Limited_Company ?x)
                (Company ?x)))
     
     (rule* subClass_120
            (if	(Act_of_Law ?x)
                (Legal_Speech_Act ?x)))
     
     (rule* subClass_121
            (if	(Mandatory_Precedent ?x)
                (Precedent ?x)))
     
     (rule* subClass_122
            (if	(Benefits ?x)
                (rev_lkif_Action ?x)))
     
     (rule* subClass_123
            (if	(Proposition ?x)
                (Mental_Object ?x)))
     
     (rule* subClass_124
            (if	(EU_Community ?x)
                (Legislative_Body ?x)))
     
     (rule* subClass_125
            (if	(Public_Act ?x)
                (Action ?x)))
     
     (rule* subClass_126
            (if	(Security ?x)
                (Tangible_Asset ?x)))
     
     (rule* subClass_127
            (if	(Non_Compliance ?x)
                (Legal_Case_Assessment ?x)))
     
     (rule* subClass_128
            (if	(Person_Role ?x)
                (Social_Role ?x)))
     
     (rule* subClass_129
            (if	(Competitive_Strength ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_130
            (if	(Cash ?x)
                (Artifact ?x)))
     
     (rule* subClass_131
            (if	(Obliged ?x)
                (Normative_Propositional_Attitude ?x)))
     
     (rule* subClass_132
            (if	(National_Law ?x)
                (Law ?x)))
     
     (rule* subClass_133
            (if	(Exchange_of_Securities ?x)
                (Exchange ?x)))
     
     (rule* subClass_134
            (if	(Fact ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_135
            (if	(Representation_of_Employees ?x)
                (Right ?x)))
     
     (rule* subClass_136
            (if	(Code_of_Conduct ?x)
                (Soft_Law ?x)))
     
     (rule* subClass_137
            (if	(Refuse ?x)
                (Action ?x)))
     
     (rule* subClass_138
            (if	(Nominal_Value ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_139
            (if	(Incorporated ?x)
                (Corporation ?x)))
     
     (rule* subClass_140
            (if	(Registered_Office ?x)
                (Office ?x)))
     
     (rule* subClass_141
            (if	(Attribute ?x)
                (Action ?x)))
     
     (rule* subClass_142
            (if	(European_Company ?x)
                (Company ?x)))
     
     (rule* subClass_143
            (if	(Recieving_Company ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_144
            (if	(Commission ?x)
                (Public_Body ?x)))
     
     (rule* subClass_145
            (if	(Assumption ?x)
                (Atom ?x)))
     
     (rule* subClass_146
            (if	(Declarative_Power ?x)
                (Potestative_Expression ?x)))
     
     (rule* subClass_147
            (if	(Decree ?x)
                (Legal_Document ?x)))
     
     (rule* subClass_148
            (if	(Money ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_149
            (if	(Role ?x)
                (Mental_Concept ?x)))
     
     (rule* subClass_150
            (if	(Assertion ?x)
                (Communicated_Attitude ?x)))
     
     (rule* subClass_151
            (if	(Permission ?x)
                (Norm ?x)))
     
     (rule* subClass_152
            (if	(Belief_In_Violation ?x)
                (Belief ?x)))
     
     (rule* subClass_153
            (if	(Tax_Regime ?x)
                (Legal_Source ?x)))
     
     (rule* subClass_154
            (if	(Registered ?x)
                (Qualification ?x)))
     
     (rule* subClass_155
            (if	(Moment ?x)
                (Atom ?x)))
     
     (rule* subClass_156
            (if	(Speech_Act ?x)
                (Creation ?x)))
     
     (rule* subClass_157
            (if	(Loss ?x)
                (Financial_Role ?x)))
     
     (rule* subClass_158
            (if	(Continuation ?x)
                (Change ?x)))
     
     (rule* subClass_159
            (if	(Collaborative_Plan ?x)
                (Plan ?x)))
     
     (rule* subClass_160
            (if	(Employee ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_161
            (if	(Disallowed ?x)
                (Normatively_Qualified ?x)))
     
     (rule* subClass_162
            (if	(Congress ?x)
                (Legislative_Body ?x)))
     
     (rule* subClass_163
            (if	(Evaluative_Expression ?x)
                (Evaluative_Proposition ?x)))
     
     (rule* subClass_164
            (if	(Epistemic_Role ?x)
                (Role ?x)))
     
     (rule* subClass_165
            (if	(Countable_Value ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_166
            (if	(Potestative_Expression ?x)
                (Legal_Expression ?x)))
     
     (rule* subClass_167
            (if	(Precedent ?x)
                (Legal_Source ?x)))
     
     (rule* subClass_168
            (if	(Acquired_Company ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_169
            (if	(Reason ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_170
            (if	(Paragraph ?x)
                (Medium ?x)))
     
     (rule* subClass_171
            (if	(Soft_Law ?x)
                (Legal_Source ?x)))
     
     (rule* subClass_172
            (if	(Pair ?x)
                (Composition ?x)))
     
     (rule* subClass_173
            (if	(Composition ?x)
                (Whole ?x)))
     
     (rule* subClass_174
            (if	(Transfer_Of_Assets ?x)
                (Exchange ?x)))
     
     (rule* subClass_175
            (if	(Person ?x)
                (Agent ?x)))
     
     (rule* subClass_176
            (if	(Holding_Company ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_177
            (if	(Profit ?x)
                (Gain ?x)))
     
     (rule* subClass_178
            (if	(Contract ?x)
                (Legal_Document ?x)))
     
     (rule* subClass_179
            (if	(Evaluative_Attitude ?x)
                (Propositional_Attitude ?x)))
     
     (rule* subClass_180
            (if	(Council ?x)
                (Legislative_Body ?x)))
     
     (rule* subClass_181
            (if	(Temporal_Occurrence ?x)
                (Spatio_Temporal_Occurrence ?x)))
     
     (rule* subClass_182
            (if	(Observation_of_Violation ?x)
                (Problem ?x)))
     
     (rule* subClass_183
            (if	(Market ?x)
                (Place ?x)))
     
     (rule* subClass_184
            (if	(Deduction ?x)
                (Action ?x)))
     
     (rule* subClass_185
            (if	(Action_Power ?x)
                (Hohfeldian_Power ?x)))
     
     (rule* subClass_186
            (if	(Argument ?x)
                (Rule ?x)))
     
     (rule* subClass_187
            (if	(Community_Law_Provision ?x)
                (Provision ?x)))
     
     (rule* subClass_188
            (if	(Person ?x)
                (Natural_Object ?x)))
     
     (rule* subClass_189
            (if	(Qualification ?x)
                (Mental_Object ?x)))
     
     (rule* subClass_190
            (if	(Atom ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_191
            (if	(Relative_Place ?x)
                (Place ?x)))
     
     (rule* subClass_192
            (if	(Legal_Expression ?x)
                (Expression ?x)))
     
     (rule* subClass_193
            (if	(Permitted ?x)
                (Normative_Propositional_Attitude ?x)))
     
     (rule* subClass_194
            (if	(Internal_Market ?x)
                (Market ?x)))
     
     (rule* subClass_195
            (if	(Selling ?x)
                (Exchange ?x)))
     
     (rule* subClass_196
            (if	(Buying ?x)
                (Exchange ?x)))
     
     (rule* subClass_197
            (if	(Corporation ?x)
                (Private_Legal_Person ?x)))
     
     (rule* subClass_198
            (if	(Promise ?x)
                (Communicated_Attitude ?x)))
     
     (rule* subClass_199
            (if	(Hohfeldian_Power ?x)
                (Potestative_Expression ?x)))
     
     (rule* subProperty_0
            (if	(employs ?x ?y)
                (role_relationship ?x ?y)))
     
     (rule* subClass_200
            (if	(Tax_Exemption ?x)
                (Exemption ?x)))
     
     (rule* subClass_201
            (if	(Company ?x)
                (Private_Legal_Person ?x)))
     
     (rule* subClass_202
            (if	(Liberty_Right ?x)
                (Right ?x)))
     
     (rule* subClass_203
            (if	(Declaration ?x)
                (Communicated_Attitude ?x)))
     
     (rule* subClass_204
            (if	(Non-binding_International_Agreement ?x)
                (International_Agreement ?x)))
     
     (rule* subClass_205
            (if	(Tax_Evasion ?x)
                (Action ?x)))
     
     (rule* subClass_206
            (if	(Legal_Doctrine ?x)
                (Legal_Source ?x)))
     
     (rule* subClass_207
            (if	(Common_Tax_System ?x)
                (Legal_Source ?x)))
     
     (rule* subClass_208
            (if	(Artifact ?x)
                (Physical_Object ?x)))
     
     (rule* subClass_209
            (if	(Taxation ?x)
                (rev_lkif_Action ?x)))
     
     (rule* subClass_210
            (if	(Title ?x)
                (Expression ?x)))
     
     (rule* subClass_211
            (if	(Evaluative_Expression ?x)
                (Legal_Expression ?x)))
     
     (rule* subClass_212
            (if	(Apply ?x)
                (Action ?x)))
     
     (rule* subClass_213
            (if	(Double_Taxation_Agreement ?x)
                (International_Agreement ?x)))
     
     (rule* subClass_214
            (if	(Intention ?x)
                (Propositional_Attitude ?x)))
     
     (rule* subClass_215
            (if	(Obliged ?x)
                (Allowed ?x)))
     
     (rule* subClass_216
            (if	(Accounting_Par_Value ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_217
            (if	(Allowed ?x)
                (Normatively_Qualified ?x)))
     
     (rule* subClass_218
            (if	(Company ?x)
                (Company ?x)))
     
     (rule* subClass_219
            (if	(Exception ?x)
                (Atom ?x)))
     
     (rule* subClass_220
            (if	(Mandate ?x)
                (Public_Act ?x)))
     
     (rule* subClass_221
            (if	(Business ?x)
                (Private_Legal_Person ?x)))
     
     (rule* subClass_222
            (if	(Exchange_of_Shares ?x)
                (Exchange ?x)))
     
     (rule* subClass_223
            (if	(Legal_Person ?x)
                (Organisation ?x)))
     
     (rule* subClass_224
            (if	(Statement_In_Writing ?x)
                (Communicated_Attitude ?x)))
     
     (rule* subClass_225
            (if	(Parliament ?x)
                (Legislative_Body ?x)))
     
     (rule* subClass_226
            (if	(Negated_Atom ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_227
            (if	(Transaction ?x)
                (Collaborative_Plan ?x)))
     
     (rule* subClass_228
            (if	(Definitional_Expression ?x)
                (Legal_Expression ?x)))
     
     (rule* subClass_229
            (if	(Part ?x)
                (Abstract_Concept ?x)))
     
     (rule* subClass_230
            (if	(Reserved_Capital ?x)
                (Nominal_Capital ?x)))
     
     (rule* subClass_231
            (if	(Wealth ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_232
            (if	(Company_from_a_Member_State ?x)
                (Company ?x)))
     
     (rule* subClass_233
            (if	(Code ?x)
                (Legal_Document ?x)))
     
     (rule* subClass_234
            (if	(Professional_Legal_Role ?x)
                (Social_Legal_Role ?x)))
     
     (rule* subClass_235
            (if	(Transaction ?x)
                (rev_lkif_Action ?x)))
     
     (rule* subClass_236
            (if	(Opinion ?x)
                (Evaluative_Proposition ?x)))
     
     (rule* subClass_237
            (if	(Dollar ?x)
                (Unit_of_Measure ?x)))
     
     (rule* subClass_238
            (if	(Exception ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_239
            (if	(Legal_Case_Assessment ?x)
                (Propositional_Attitude ?x)))
     
     (rule* subClass_240
            (if	(Committee ?x)
                (Legislative_Body ?x)))
     
     (rule* subClass_241
            (if	(Association ?x)
                (Private_Legal_Person ?x)))
     
     (rule* subClass_242
            (if	(Voting_Right ?x)
                (Permissive_Right ?x)))
     
     (rule* subClass_243
            (if	(Transferring_Company ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_244
            (if	(Location_Complex ?x)
                (Place ?x)))
     
     (rule* subClass_245
            (if	(Assumption ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_246
            (if	(Strictly_Disallowed ?x)
                (Disallowed ?x)))
     
     (rule* subClass_247
            (if	(Representative ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_248
            (if	(Address ?x)
                (rev_lkif_Action ?x)))
     
     (rule* subClass_249
            (if	(Rationalize ?x)
                (Action ?x)))
     
     (rule* subClass_250
            (if	(Belief ?x)
                (Propositional_Attitude ?x)))
     
     (rule* subClass_251
            (if	(Reserve ?x)
                (Tangible_Asset ?x)))
     
     (rule* subClass_252
            (if	(Immunity ?x)
                (Hohfeldian_Power ?x)))
     
     (rule* subClass_253
            (if	(Act_of_Law ?x)
                (Public_Act ?x)))
     
     (rule* subClass_254
            (if	(Partial_Division ?x)
                (European_Economic_Operation ?x)))
     
     (rule* subClass_255
            (if	(Private_Legal_Person ?x)
                (Legal_Person ?x)))
     
     (rule* subClass_256
            (if	(Society ?x)
                (Private_Legal_Person ?x)))
     
     (rule* subClass_257
            (if	(Legal_Document ?x)
                (Document ?x)))
     
     (rule* subClass_258
            (if	(Allowed_And_Disallowed ?x)
                (Allowed ?x)))
     
     (rule* subClass_259
            (if	(Area ?x)
                (Natural_Object ?x)))
     
     (rule* subClass_260
            (if	(Depreciation ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_261
            (if	(Independent_Business ?x)
                (Business ?x)))
     
     (rule* subClass_262
            (if	(Document ?x)
                (Medium ?x)))
     
     (rule* subClass_263
            (if	(Ownership ?x)
                (Mental_Object ?x)))
     
     (rule* subClass_264
            (if	(Social_Role ?x)
                (Role ?x)))
     
     (rule* subClass_265
            (if	(Capital_Gain ?x)
                (Gain ?x)))
     
     (rule* subClass_266
            (if	(Whole ?x)
                (Abstract_Concept ?x)))
     
     (rule* subClass_267
            (if	(Office ?x)
                (Place ?x)))
     
     (rule* subClass_268
            (if	(EU_Directive_31990L0434 ?x)
                (Directive ?x)))
     
     (rule* subClass_269
            (if	(Stock ?x)
                (Tangible_Asset ?x)))
     
     (rule* subProperty_1
            (if	(employed_by ?x ?y)
                (role_relationship ?x ?y)))
     
     (rule* subClass_270
            (if	(International_Agreement ?x)
                (Legal_Source ?x)))
     
     (rule* subClass_271
            (if	(Legal_Document ?x)
                (Legal_Source ?x)))
     
     (rule* subClass_272
            (if	(Jurisdictive_Power ?x)
                (Potestative_Expression ?x)))
     
     (rule* subClass_273
            (if	(Communicate ?x)
                (rev_lkif_Action ?x)))
     
     (rule* subClass_274
            (if	(Community_Level ?x)
                (Business_Level ?x)))
     
     (rule* subClass_275
            (if	(European_Cooperative_Societies ?x)
                (Society ?x)))
     
     (rule* subClass_276
            (if	(Transfer ?x)
                (Action ?x)))
     
     (rule* subClass_277
            (if	(Exhaustion ?x)
                (Action ?x)))
     
     (rule* subClass_278
            (if	(Proclamation ?x)
                (Legal_Source ?x)))
     
     (rule* subClass_279
            (if	(Asset ?x)
                (Economic_Role ?x)))
     
     (rule* subClass_280
            (if	(Persuasive_Precedent ?x)
                (Precedent ?x)))
     
     (rule* subClass_281
            (if	(Common_Market ?x)
                (Market ?x)))
     
     (rule* subClass_282
            (if	(Assignment ?x)
                (Public_Act ?x)))
     
     (rule* subClass_283
            (if	(Employer ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_284
            (if	(Productivity ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_285
            (if	(Legal_Role ?x)
                (Role ?x)))
     
     (rule* subClass_286
            (if	(Initiation ?x)
                (Change ?x)))
     
     (rule* subClass_287
            (if	(Cash_Payment ?x)
                (Payment ?x)))
     
     (rule* subClass_288
            (if	(Comprehensive_Place ?x)
                (Location_Complex ?x)))
     
     (rule* subClass_289
            (if	(Unincorporated ?x)
                (Corporation ?x)))
     
     (rule* subClass_290
            (if	(Regulation ?x)
                (Legal_Document ?x)))
     
     (rule* subClass_291
            (if	(Function ?x)
                (Role ?x)))
     
     (rule* subClass_292
            (if	(Social_Legal_Role ?x)
                (Social_Role ?x)))
     
     (rule* subClass_293
            (if	(Indirect_Shareholder ?x)
                (Shareholder ?x)))
     
     (rule* subClass_294
            (if	(Enabling_Power ?x)
                (Potestative_Expression ?x)))
     
     (rule* subClass_295
            (if	(Liability_Right ?x)
                (Right ?x)))
     
     (rule* subClass_296
            (if	(Disallowed_Intention ?x)
                (Intention ?x)))
     
     (rule* subClass_297
            (if	(Action ?x)
                (Process ?x)))
     
     (rule* subClass_298
            (if	(Permanent_Establishment ?x)
                (Legal_Role ?x)))
     
     (rule* subClass_299
            (if	(Restructuring ?x)
                (Action ?x)))
     
     (rule* subClass_300
            (if	(Law ?x)
                (Norm ?x)))
     
     (rule* subClass_301
            (if	(Tangible_Liability ?x)
                (Liability ?x)))
     
     (rule* subClass_302
            (if	(Real_Value ?x)
                (Value_Measure ?x)))
     
     (rule* subClass_303
            (if	(Rule ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_304
            (if	(Requirement ?x)
                (Obligation ?x)))
     
     (rule* subClass_305
            (if	(Compliance ?x)
                (Legal_Case_Assessment ?x)))
     
     (rule* subClass_306
            (if	(Atom ?x)
                (Abstract_Concept ?x)))
     
     (rule* subClass_307
            (if	(Delegation ?x)
                (Legal_Speech_Act ?x)))
     
     (rule* subClass_308
            (if	(Gain ?x)
                (Financial_Role ?x)))
     
     (rule* subClass_309
            (if	(Valid_Commercial_Reason ?x)
                (Reason ?x)))
     
     (rule* subClass_310
            (if	(International_Arbitration ?x)
                (Soft_Law ?x)))
     
     (rule* subClass_311
            (if	(Evidence ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_312
            (if	(Qualificatory_Expression ?x)
                (Legal_Expression ?x)))
     
     (rule* subClass_313
            (if	(Enterprise ?x)
                (Legal_Person ?x)))
     
     (rule* subClass_314
            (if	(Intangible_Asset ?x)
                (Asset ?x)))
     
     (rule* subClass_315
            (if	(International_Level ?x)
                (Business_Level ?x)))
     
     (rule* subClass_316
            (if	(Existential_Expression ?x)
                (Legal_Expression ?x)))
     
     (rule* subClass_317
            (if	(Organisation ?x)
                (Agent ?x)))
     
     (rule* subClass_318
            (if	(Evaluative_Proposition ?x)
                (Proposition ?x)))
     
     (rule* subClass_319
            (if	(Direct_Shareholder ?x)
                (Shareholder ?x)))
     
     (rule* subClass_320
            (if	(Merger ?x)
                (European_Economic_Operation ?x)))
     
     (rule* subClass_321
            (if	(Rule ?x)
                (Norm ?x)))
     
     (rule* subClass_322
            (if	(Employee_in_a_Company_Organ ?x)
                (Employee ?x)))
     
     (rule* subClass_323
            (if	(Takeover ?x)
                (Economic_Operation ?x)))
     
     (rule* subClass_324
            (if	(Permissive_Right ?x)
                (Right ?x)))
     
     (rule* subClass_325
            (if	(Directive ?x)
                (Legal_Document ?x)))
     
     (rule* subClass_326
            (if	(Obligation ?x)
                (Permission ?x)))
     
     (rule* subClass_327
            (if	(Division ?x)
                (European_Economic_Operation ?x)))
     
     (rule* subClass_328
            (if	(Decision ?x)
                (Legal_Speech_Act ?x)))
     
     (rule* subClass_329
            (if	(Issued_Capital ?x)
                (Nominal_Capital ?x)))
     
     (rule* subClass_330
            (if	(Comply ?x)
                (rev_lkif_Action ?x)))
     
     (rule* subClass_331
            (if	(Economic_Operation ?x)
                (rev_lkif_Action ?x)))
     
     (rule* subClass_332
            (if	(Expectation ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_333
            (if	(Trade ?x)
                (Transaction ?x)))
     
     (rule* subClass_334
            (if	(Natural_Person ?x)
                (Person ?x)))
     
     (rule* subClass_335
            (if	(Observation ?x)
                (Epistemic_Role ?x)))
     
     (rule* subClass_336
            (if	(Legal_Source ?x)
                (Medium ?x)))
     
     (rule* subClass_337
            (if	(Treaty ?x)
                (International_Agreement ?x)))
     
     
     
     ;"defeasible" normative rules
     
     (rule art.10.1.1 (if (and (Permanent_Establishment ?x) 
                               (Transferring_Company ?y)    
                               (Member_State ?p)            
                               (situated_in ?y ?p)
                               (Member_State ?q)
                               (situated_in ?x ?q)
                               (Merger ?a)
                               (requirement_of ?x ?a)
                               (actor_in ?y ?a)
                               (Taxation ?t)  
                               (actor_in ?p ?t)  
                               (requirement_of ?x ?t))
                          (Disallowed ?t)))
     
     (rule art.10d (if (and (European_Company ?a)
                            (Transfer ?b)
                            (actor_in ?a ?b)
                            (Registered_Office ?c)
                            (requirement_of ?c ?b)
                            (Member_State ?d)
                            (Capital ?capx)
                            (plays ?cap ?capx)
                            (possesion_of ?cap ?a)
                            (Gain ?g1)
                            (plays ?g ?g1)
                            (Taxation ?t)
                            (Shareholder ?s1)
                            (plays ?s ?s1)
                            (Share ?shx)
                            (plays ?sh ?shx)
                            (owns ?s ?sh) 
                            (represents ?sh ?cap) 
                            (requirement_of ?g ?t) 
                            (actor_in ?d ?t))                        
                       (Disallowed ?t)))
     
     
     
     ;"strict" facts
     (rule* facts 	
            (Permanent_Establishment Barneys)  
            (Transferring_Company Gonnies) 
            (Member_State Germany) 
            (situated_in Gonnies Germany)
            (Member_State Netherlands)
            (situated_in Barneys Netherlands)
            (Merger merger2007)
            (requirement_of Barneys merger2007)
            (actor_in Gonnies merger2007)
            (Taxation taxation1)  
            (actor_in Germany taxation1)  
            (requirement_of Barneys taxation1)
            
            (European_Company Stix)
            (Payment payment1)
            (actor_in Stix payment1)
            (Registered_Office office1)
            (requirement_of office1 payment1)
            (Member_State Poland)
            (Reserved_Capital r_capital)
            (plays capital r_capital)
            (possesion_of capital Stix)
            (Capital_Gain c_gain1)
            (plays gain c_gain1)
            (Taxation taxation2)
            (Shareholder main_sh)
            (plays John main_sh)
            (Share shares)
            (plays papers shares)
            (owns John papers)
            (represents papers capital)
            (requirement_of gain taxation2) 
            (actor_in Poland taxation2))
     
     )) ; end of rule base
  
  ; type question = excluded | priority | valid
  
  ; engine integer integer (list-of symbol) -> statement -> (stream-of argument-state)
  (define (engine max-nodes max-turns critical-questions)
    (make-engine max-nodes max-turns 
                 (list (generate-arguments-from-rules rb1 critical-questions) builtins)))
  
  ; queries
  ;  (ask1 '(Disallowed ?x) (engine 100 2 null)) 
  ; (show1 '(Disallowed ?x) (engine 100 2 null))
  (ask '(Disallowed ?x) (engine 100 2 null))
  )  