#include "EmitIR.hpp"
#include <llvm/Transforms/Utils/ModuleUtils.h>

#define self (*this)

using namespace asg;

EmitIR::EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid)
  : mMgr(mgr)
  , mMod(mid, ctx)
  , mCtx(ctx)
  , mIntTy(llvm::Type::getInt32Ty(ctx))
  , mCurIrb(std::make_unique<llvm::IRBuilder<>>(ctx))
  , mCtorTy(llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), false))
{
}

llvm::Module&
EmitIR::operator()(asg::TranslationUnit* tu)
{
  for (auto&& i : tu->decls)
    self(i);
  return mMod;
}

//==============================================================================
// 类型
//==============================================================================

llvm::Type*
EmitIR::operator()(const Type* type)
{
  if (type->texp == nullptr) {
    switch (type->spec) {
      case Type::Spec::kInt:
        return llvm::Type::getInt32Ty(mCtx);
      // TODO: 在此添加对更多基础类型的处理
      case Type::Spec::kVoid:
        return llvm::Type::getVoidTy(mCtx);
      
      default:
        ABORT();
    }
  }

  Type subt;
  subt.spec = type->spec;
  subt.qual = type->qual;
  subt.texp = type->texp->sub;

  // TODO: 在此添加对指针类型、数组类型和函数类型的处理

  if (auto p = type->texp->dcst<FunctionType>()) {
    std::vector<llvm::Type*> pty;
    return llvm::FunctionType::get(self(&subt), std::move(pty), false);
  }
  if (auto p = type->texp->dcst<PointerType>()) {
    llvm::Type* elementType = self(&subt);
    return llvm::PointerType::get(elementType, 0); // 0 表示地址空间，通常为默认值
  }
  if (auto p = type->texp->dcst<ArrayType>()) {
    llvm::Type* elementType = self(&subt);
    return llvm::ArrayType::get(elementType, p->len); // 假设 ArrayType 有一个 size 成员表示数组大小
  }

  ABORT();
}

//==============================================================================
// 表达式
//==============================================================================

llvm::Value*
EmitIR::operator()(Expr* obj)
{
  // TODO: 在此添加对更多表达式处理的跳转
  if (auto p = obj->dcst<IntegerLiteral>())
    return self(p);
  
  if (auto p = obj->dcst<DeclRefExpr>())
    return self(p);

  if (auto p = obj->dcst<ImplicitCastExpr>())
    return self(p);

  if (auto p = obj->dcst<BinaryExpr>())
    return self(p);

  if (auto p = obj->dcst<UnaryExpr>())
    return self(p);

  if (auto p = obj->dcst<ParenExpr>())
    return self(p);

  if (auto p = obj->dcst<CallExpr>())
    return self(p);
  
  if (auto p = obj->dcst<InitListExpr>())
    return self(p);

  ABORT();
}

llvm::Constant*
EmitIR::operator()(IntegerLiteral* obj)
{
  return llvm::ConstantInt::get(self(obj->type), obj->val);
}

// TODO: 在此添加对更多表达式类型的处理
llvm::Value*
EmitIR::operator()(ImplicitCastExpr* obj)
{
  auto sub = self(obj->sub);

  auto& irb = *mCurIrb;
  switch (obj->kind) {
    case ImplicitCastExpr::kLValueToRValue: {
      auto ty = self(obj->sub->type);
      auto loadVal = irb.CreateLoad(ty, sub);
      return loadVal;
    }
    case ImplicitCastExpr::kFunctionToPointerDecay: {
      llvm::Type* pointerType = sub->getType()->getPointerTo();
      return irb.CreatePointerCast(sub, pointerType);
      //return sub;
    }

    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(DeclRefExpr* obj)
{
  // 在LLVM IR层面，左值体现为返回指向值的指针
  // 在ImplicitCastExpr::kLValueToRValue中发射load指令从而变成右值
  return reinterpret_cast<llvm::Value*>(obj->decl->any);
}

llvm::Value*
EmitIR::operator()(BinaryExpr* obj)
{
  auto& irb = *mCurIrb;
  llvm::Type *Int32Ty = llvm::Type::getInt32Ty(irb.getContext());
  llvm::Value *lftVal, *rhtVal;
  lftVal = self(obj->lft);
  // rhtVal = self(obj->rht);
  switch (obj->op) {
    case BinaryExpr::kAdd:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateAdd(lftVal, rhtVal);}
    case BinaryExpr::kSub:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateSub(lftVal, rhtVal);}
    case BinaryExpr::kMul:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateMul(lftVal, rhtVal);}
    case BinaryExpr::kDiv:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateSDiv(lftVal, rhtVal);}
    case BinaryExpr::kMod:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateSRem(lftVal, rhtVal);}
    case BinaryExpr::kGt:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateICmpSGT(lftVal, rhtVal);}
    case BinaryExpr::kGe:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateICmpSGE(lftVal, rhtVal);}
    case BinaryExpr::kLt:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateICmpSLT(lftVal, rhtVal);}
    case BinaryExpr::kLe:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateICmpSLE(lftVal, rhtVal);}
    case BinaryExpr::kEq:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateICmpEQ(lftVal, rhtVal);}
    case BinaryExpr::kNe:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      return irb.CreateICmpNE(lftVal, rhtVal);}
    case BinaryExpr::kAssign:{
      // llvm::Value *lftVal, *rhtVal;
      // lftVal = self(obj->lft);
      rhtVal = self(obj->rht);
      irb.CreateStore(rhtVal, lftVal);
      return rhtVal;}
    case BinaryExpr::kAnd:{
      llvm::BasicBlock* originalBB = mCurIrb->GetInsertBlock(); // 保存原始基本块指针
      llvm::BasicBlock* exp1BB = llvm::BasicBlock::Create(mCtx, "land.exp1", mCurFunc);
      llvm::BasicBlock* exp2BB = llvm::BasicBlock::Create(mCtx, "land.exp2", mCurFunc);
      llvm::BasicBlock* endBB = llvm::BasicBlock::Create(mCtx, "land.end", mCurFunc);

      mCurIrb->CreateBr(exp1BB);

      irb.SetInsertPoint(exp1BB);
      llvm::Value* lftBool = mCurIrb->CreateICmpNE(lftVal, llvm::ConstantInt::get(lftVal->getType(), 0));
      // mCurIrb->CreateCondBr(lftVal, exp2BB, endBB);
      irb.CreateCondBr(lftBool, exp2BB, endBB);
      irb.SetInsertPoint(exp2BB);

      rhtVal = self(obj->rht);
      llvm::Value* rhtBool = mCurIrb->CreateICmpNE(rhtVal, llvm::ConstantInt::get(rhtVal->getType(), 0));
      llvm::BasicBlock *curBlock = mCurIrb->GetInsertBlock();
      mCurIrb->CreateBr(endBB);

      irb.SetInsertPoint(endBB);
      // mCurIrb = std::make_unique<llvm::IRBuilder<>>(endBB);
      llvm::PHINode* phiNode = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2,"merge");
      // 如果从处理 exp_1 的基本块跳转过来，表达式的值为 false
      phiNode->addIncoming(irb.getInt1(false), exp1BB);
      //phiNode->addIncoming(irb.getInt32(0), exp1BB);
      // 如果从处理 exp_2 的基本块跳转过来，表达式的值与 exp_2 的值一样
      phiNode->addIncoming(rhtBool, curBlock);
      // mCurIrb = std::make_unique<llvm::IRBuilder<>>(originalBB);
      return phiNode;   
      
    }
   
   case BinaryExpr::kOr:{
      llvm::BasicBlock* originalBB = mCurIrb->GetInsertBlock(); // 保存原始基本块指针
      llvm::BasicBlock* exp1BB = llvm::BasicBlock::Create(mCtx, "lor.exp1", mCurFunc);
      llvm::BasicBlock* exp2BB = llvm::BasicBlock::Create(mCtx, "lor.exp2", mCurFunc);
      llvm::BasicBlock* endBB = llvm::BasicBlock::Create(mCtx, "lor.end", mCurFunc);
      mCurIrb->CreateBr(exp1BB);
      
      mCurIrb = std::make_unique<llvm::IRBuilder<>>(exp1BB);
      llvm::Value* lftBool = mCurIrb->CreateICmpNE(lftVal, llvm::ConstantInt::get(lftVal->getType(), 0));
      mCurIrb->CreateCondBr(lftBool, endBB, exp2BB); // 如果 exp_1 为 true，则跳转到 endBB，否则跳转到 exp2BB
      mCurIrb = std::make_unique<llvm::IRBuilder<>>(exp2BB);
      rhtVal = self(obj->rht);
      llvm::Value* rhtBool = mCurIrb->CreateICmpNE(rhtVal, llvm::ConstantInt::get(rhtVal->getType(), 0));
      llvm::BasicBlock *curBlock = mCurIrb->GetInsertBlock();
      mCurIrb->CreateBr(endBB);
      mCurIrb = std::make_unique<llvm::IRBuilder<>>(endBB);
      llvm::PHINode* phiNode = mCurIrb->CreatePHI(llvm::Type::getInt1Ty(mCtx), 2);
      // 如果从处理 exp_1 的基本块跳转过来，表达式的值为 true
      phiNode->addIncoming(llvm::ConstantInt::getTrue(mCtx), exp1BB);
      //如果从处理 exp_2 的基本块跳转过来，表达式的值与 exp_2 的值一样
      phiNode->addIncoming(rhtBool, curBlock);

      return phiNode;
    }
    // case BinaryExpr::kIndex:{


    // }
    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(UnaryExpr* obj)
{
  llvm::Value *Val = self(obj->sub);
  auto& irb = *mCurIrb;
  llvm::Type *Int32Ty = llvm::Type::getInt32Ty(irb.getContext());
  switch (obj->op) {

    case UnaryExpr::kNot:{
      llvm::Value* toBool = irb.CreateICmpNE(Val, irb.getInt32(0), "tobool");
      llvm::Value* lNot = irb.CreateXor(toBool, irb.getInt1(true), "lnot");
      return irb.CreateZExt(lNot, Int32Ty, "lnot.ext");
    }
      
    case UnaryExpr::kNeg:{
        return irb.CreateNeg(Val, "neg_tmp");
        // llvm::Value* neg = irb.CreateNSWSub(irb.getInt32(0), Val, "neg");
      }

    case UnaryExpr::kPos:
        return Val;
    default:
      ABORT();

    
  }
}

llvm::Value*
EmitIR::operator()(ParenExpr* obj){
  return self(obj->sub);
}

llvm::Value*
EmitIR::operator()(CallExpr* obj){
  
  auto& irb = *mCurIrb;
  llvm::Function *func = mMod.getFunction(self(obj->head)->getName());
  std::vector<llvm::Value*> ParamValues;
  llvm::Value* result;
  if(!(obj->args.empty())){
    for(auto& arg : obj->args){
      ParamValues.push_back(self(arg));
    }
    auto PV = llvm::ArrayRef(ParamValues);
    result = irb.CreateCall(func, PV);
  }
  else result = irb.CreateCall(func);

  return result;

}

llvm::Value*
EmitIR::operator()(InitListExpr* obj){

  // 假设 InitListExpr 有一个方法 elements() 返回包含所有初始化表达式的列表
  const auto& initList = obj->list;

  // 假设我们要初始化的目标类型已经确定，并存储在 obj->getType()
  llvm::Type* targetType = self(obj->type);

  // 检查目标类型是否为数组类型
  if (targetType->isArrayTy()) {
    llvm::ArrayType* arrayType = llvm::cast<llvm::ArrayType>(targetType);
    llvm::Type* elementType = arrayType->getElementType();
    uint64_t numElements = arrayType->getNumElements();

    // 创建一个未初始化的数组
    llvm::Value* arrayAlloc = llvm::UndefValue::get(arrayType);

    // 初始化数组的每个元素
    for (size_t i = 0; i < initList.size(); ++i) {
      llvm::Value* elementValue = (*this)(initList[i]); // 递归调用处理每个初始化表达式
      if (elementValue->getType() != elementType) {
        // 如果类型不匹配，需要进行类型转换
        elementValue = mCurIrb->CreateBitCast(elementValue, elementType);
      }
      // 插入元素到数组中
      arrayAlloc = mCurIrb->CreateInsertValue(arrayAlloc, elementValue, i);
    }

    return arrayAlloc;
  }

  // 如果不是数组类型，需要添加对其他类型的处理，如结构体类型等
  // 这里只处理了数组类型作为示例

  ABORT(); // 如果遇到无法处理的类型，则终止


}





//==============================================================================
// 语句
//==============================================================================

void
EmitIR::operator()(Stmt* obj)
{
  // TODO: 在此添加对更多Stmt类型的处理的跳转

  if (auto p = obj->dcst<CompoundStmt>())
    return self(p);

  if (auto p = obj->dcst<ReturnStmt>())
    return self(p);

  if (auto p = obj->dcst<DeclStmt>())
    return self(p);

  if (auto p = obj->dcst<ExprStmt>())
    return self(p);

  if (auto p = obj->dcst<IfStmt>())
    return self(p);

  if (auto p = obj->dcst<WhileStmt>())
    return self(p);

  if (auto p = obj->dcst<BreakStmt>())
    return self(p);

  if (auto p = obj->dcst<ContinueStmt>())
    return self(p);

  if (auto p = obj->dcst<NullStmt>())
    return self(p);

  ABORT();
}

// TODO: 在此添加对更多Stmt类型的处理



void
EmitIR::operator()(CompoundStmt* obj)
{
  // TODO: 可以在此添加对符号重名的处理
  for (auto&& stmt : obj->subs)
    self(stmt);
}

void
EmitIR::operator()(ReturnStmt* obj)
{
  auto& irb = *mCurIrb;

  llvm::Value* retVal;
  if (!obj->expr)
    retVal = nullptr;
  else
    retVal = self(obj->expr);

  mCurIrb->CreateRet(retVal);

  auto exitBb = llvm::BasicBlock::Create(mCtx, "return_exit", mCurFunc);
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(exitBb);
}

void
EmitIR::operator()(DeclStmt* obj)
{
  for (auto&& decl : obj->decls)
    self(decl);
}

void
EmitIR::operator()(ExprStmt* obj)
{
  if (obj->expr)
    self(obj->expr);
}

void 
EmitIR::operator()(IfStmt* obj)
{
  auto& irb = *mCurIrb;
  // 条件表达式的 LLVM 值
  llvm::Value* conditionValue = self(obj->cond);

  // 创建 if 语句的基本块
  llvm::Function* curFunc = mCurFunc;
  llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(mCtx, "if.then", curFunc);
  llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(mCtx, "if.else", curFunc);
  llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(mCtx, "if.merge", curFunc);

  
  // 创建条件分支指令
  if(!conditionValue->getType()->isIntegerTy(1)) 
    conditionValue = mCurIrb->CreateICmpNE(conditionValue, mCurIrb->getInt32(0), "tobool");
  // llvm::Value* toBool = mCurIrb->CreateICmpNE(conditionValue, mCurIrb->getInt32(0), "tobool");
  // mCurIrb->CreateCondBr(toBool, thenBB, elseBB);
  mCurIrb->CreateCondBr(conditionValue, thenBB, elseBB);
  
  // // 生成 "then" 块中的代码
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(thenBB);
  self(obj->then);
  if (mCurIrb->GetInsertBlock()->getTerminator()==nullptr) {
        mCurIrb->CreateBr(mergeBB);
    }

  mCurIrb = std::make_unique<llvm::IRBuilder<>>(elseBB);

  if (obj->else_){
    self(obj->else_);
  }

  if (mCurIrb->GetInsertBlock()->getTerminator()==nullptr) {
        mCurIrb->CreateBr(mergeBB);

    }

  mCurIrb = std::make_unique<llvm::IRBuilder<>>(mergeBB);






}

void 
EmitIR::operator()(WhileStmt* obj)
{

    auto& irb = *mCurIrb;

    // 创建循环的基本块
    llvm::Function* curFunc = mCurFunc;
    llvm::BasicBlock* loopEntryBB = llvm::BasicBlock::Create(mCtx, "while.entry", curFunc);
    llvm::BasicBlock* loopBodyBB = llvm::BasicBlock::Create(mCtx, "while.body", curFunc);
    llvm::BasicBlock* loopEndBB = llvm::BasicBlock::Create(mCtx, "while.end", curFunc);
    obj->any = loopBodyBB;
    // 跳转到循环入口
    mCurIrb->CreateBr(loopEntryBB);

    // 在循环入口设置 IR 插入点
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(loopEntryBB);

    // 创建条件表达式的 LLVM 值
    llvm::Value* conditionValue = self(obj->cond);
    if(!conditionValue->getType()->isIntegerTy(1)) 
      conditionValue = mCurIrb->CreateICmpNE(conditionValue, mCurIrb->getInt32(0), "tobool");
    // llvm::Value* toBool = mCurIrb->CreateICmpNE(conditionValue, mCurIrb->getInt32(0), "tobool");
    // 创建条件分支指令，根据条件值决定跳转到循环体或者结束循环
    // mCurIrb->CreateCondBr(toBool, loopBodyBB, loopEndBB);
    mCurIrb->CreateCondBr(conditionValue, loopBodyBB, loopEndBB);

    // 在循环体中生成代码
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(loopBodyBB);
    self(obj->body);

    // 循环体结束后，跳转回循环入口
    mCurIrb->CreateBr(loopEntryBB);

    // 在循环结束基本块设置 IR 插入点
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(loopEndBB);
}

void 
EmitIR::operator()(BreakStmt* obj)
{
    auto& irb = *mCurIrb;

    auto loopBodyBB = reinterpret_cast<llvm::BasicBlock *>(obj->loop->any);
    llvm::Function* mFunc = mCurFunc;
    for (auto i = mFunc->begin(); i != mFunc->end(); i++){
      if(&(*i) == loopBodyBB){
        irb.CreateBr(&(*(++i)));
        break;
      }
    }

}

void 
EmitIR::operator()(ContinueStmt* obj)
{
    auto& irb = *mCurIrb;

    auto loopBodyBB = reinterpret_cast<llvm::BasicBlock *>(obj->loop->any);
    llvm::Function* mFunc = mCurFunc;
    for (auto i = mFunc->begin(); i != mFunc->end(); i++){
      if(&(*i) == loopBodyBB){
        irb.CreateBr(&(*(--i)));
        break;
      }
    }
}

void 
EmitIR::operator()(NullStmt* obj){

}

//==============================================================================
// 声明
//==============================================================================

void
EmitIR::operator()(Decl* obj)
{
  // TODO: 添加变量声明处理的跳转
  if (auto p = obj->dcst<VarDecl>())
    return self(p);
  if (auto p = obj->dcst<FunctionDecl>())
    return self(p);

  ABORT();
}

// TODO: 添加变量声明的处理

void
EmitIR::trans_init(llvm::Value* val, Expr* obj)
{
  auto& irb = *mCurIrb;

  // 仅处理整数字面量的初始化
  if (auto p = obj->dcst<IntegerLiteral>()) {
    auto initVal = llvm::ConstantInt::get(self(p->type), p->val);
    irb.CreateStore(initVal, val);
    return;
  }

  // 如果表达式不是整数字面量，则中断编译
  ABORT();
}

void
EmitIR::operator()(VarDecl* obj)
{
  if(mCurFunc == nullptr){
    auto ty = llvm::Type::getInt32Ty(mCtx); // 直接使用 LLVM 的 int32 类型
    auto gvar = new llvm::GlobalVariable(
      mMod, ty, false, llvm::GlobalVariable::ExternalLinkage, nullptr, obj->name);

    obj->any = gvar;

    // 默认初始化为 0
    gvar->setInitializer(llvm::ConstantInt::get(ty, 0));

    if (obj->init == nullptr)
      return;

    // 创建构造函数用于初始化
    mCurFunc = llvm::Function::Create(
      mCtorTy, llvm::GlobalVariable::PrivateLinkage, "ctor_" + obj->name, mMod);
    llvm::appendToGlobalCtors(mMod, mCurFunc, 65535);

    auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", mCurFunc);
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
    trans_init(gvar, obj->init);
    mCurIrb->CreateRet(nullptr);

    mCurFunc = nullptr;
  }

  else{
    auto& irb = *mCurIrb;
    auto lvar = irb.CreateAlloca(self(obj->type), nullptr, std::move(obj->name));
    obj->any = lvar;
    if(obj->init){
      irb.CreateStore(self(obj->init), lvar);
    }
  }


}

/*
/// 函数类型：void(int, int)
llvm::FunctionType *funcType = llvm::FunctionType::get(
    llvm::Type::getVoidTy(TheContext),
    {llvm::Type::getInt32Ty(TheContext), llvm::Type::getInt32Ty(TheContext)},
    false);

/// 创建函数
llvm::Function *func = llvm::Function::Create(
    funcType, llvm::GlobalValue::ExternalLinkage, "f", &TheModule);

*/



void
EmitIR::operator()(FunctionDecl* obj)
{
  // 创建函数
  //auto fty = llvm::dyn_cast<llvm::FunctionType>(self(obj->type));
  llvm::FunctionType *fty;
  if(!obj->params.empty()){
    std::vector<llvm::Type*> ParamTypes;
    for(auto param : obj->params){
      ParamTypes.push_back(self(param->type));
    }
    auto PT = llvm::ArrayRef(ParamTypes);
    
    auto tt = obj->type;
    Type subt;
    subt.spec = tt->spec;
    subt.qual = tt->qual;
    subt.texp = tt->texp->sub;

    fty = llvm::dyn_cast<llvm::FunctionType>(llvm::FunctionType::get(self(&subt), PT, false));
  }
  else fty = llvm::dyn_cast<llvm::FunctionType>(self(obj->type));

  auto func = llvm::Function::Create(fty, llvm::GlobalVariable::ExternalLinkage, obj->name, mMod);

  obj->any = func;

  if (obj->body == nullptr) return;
  auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", func);
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
  auto& entryIrb = *mCurIrb;

  // 对函数参数的处理
  auto func_Args = func->arg_begin();
  for(auto param : obj->params){
    auto lvar = entryIrb.CreateAlloca(func_Args->getType(), nullptr, func_Args->getName());
    param->any = lvar;
    llvm::Argument *arg = &*func_Args;
    auto Arg_value = static_cast<llvm::Value*>(arg);
    if(Arg_value){
      entryIrb.CreateStore(Arg_value, lvar);
    }
    func_Args ++;
  }
  

  // 翻译函数体
  mCurFunc = func;
  self(obj->body);
  auto& exitIrb = *mCurIrb;

  if (fty->getReturnType()->isVoidTy())
    exitIrb.CreateRetVoid();
  
  else
    exitIrb.CreateUnreachable();

  mCurFunc = nullptr;
}
